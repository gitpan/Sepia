;;; pscope.el --- Interact with Pmacs/EPL

;; Copyright (C) 2004 Sean O'Rourke.  All rights reserved, some wrongs
;; reversed.  This code is distributed under the same terms as Perl
;; itself.

;;; Commentary:

;; Primitive environment for interactive Perl development.  To use it,
;;     1) Install Emacs::EPL and Module::Info from CPAN.
;;     2) place Devel/Xref.pm somewhere in your @INC path (or edit the
;;        code below that points to it).
;;     3) put pscope.el somewhere Emacs will find it.
;;     4) In Emacs, evaluate
;;        M-x load-library <ret> pscope <ret>
;;        M-x pscope-init <ret>
;;        M-x pscope-rebuild <ret>
;; This will give you an Xref database for the EPL process.  To add
;; some of your own modules, load them, then type
;;     M-x perl-eval-buffer
;; Alternatively, you can get an interactive Perl scratchpad like the
;; *scratch* buffer by typing
;;     M-x ipl-start <ret>
;; then type the relevant "use" statements into this buffer, and
;; evaluate them by hitting C-j.

;;; Code:

(require 'perl)
(require 'epl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; "pscope" -- use Perl to find definitions and uses.

(defvar pscope-use-completion t
  "* Use completion based on Xref database.")

(defvar pscope-initializer
"
BEGIN { push @INC, \"$ENV{HOME}/src/perl\" };
use Emacs::Lisp;
use Devel::Xref;
use Module::Info;

sub _module_info($)
{
    my $m = shift;
    my $ret;
    if (-f $m) {
	$ret = Module::Info->new_from_file($m);
    } else {
	(my $file = $m) =~ s|::|/|g;
	$file .= '.pm';
	if (exists $INC{$file}) {
	    $ret = Module::Info->new_from_loaded($m);
	} else {
	    $ret = Module::Info->new_from_module($m);
	}
    }
    die \"Can't find module ``$m''\\n\" unless $ret;
    return $ret;
}

1;
")

(defun perl-name (sym)
  (substitute ?_ ?- (symbol-name sym)))

(defun pscope-init ()
"Load perl support code and start the inferior Perl process."
  (interactive)

  ;; Load perl defs:
  (perl-eval pscope-initializer 'void-context)

  ;; Create glue wrappers for Module::Info funcs.
  (dolist (x '(name version inc-dir file is-core modules-used
	       packages-inside superclasses))
    (let ((name (intern (format "pscope-module-%s" x)))
	  (pl-func
	   (perl-eval (format "sub { _module_info(shift)->%s }" (perl-name x))
		      'scalar-context)))
      (when (fboundp name) (fmakunbound name))
      (eval `(defun ,name (mod)
	       (interactive (list (pscope-interactive-arg 'module)))
	       (let ((res (perl-call ,pl-func 'scalar-context mod)))
		 (if (interactive-p)
		     (message "%s" res)
		     res))))))

  ;; Create low-level wrappers for Devel::Xref
  (dolist (x '((package-subs . "Find all subs defined in a package.")
	       (rebuild . "Build Xref database for current Perl process.")
	       (redefined . "Rebuild Xref information for a given sub.")

	       (defs . "Find all definitions of a function.")
	       (callers . "Find all callers of a function.")
	       (callees . "Find all functions called by a function.")

	       (apropos . "Find subnames matching RE.")
	       (var-apropos . "Find varnames matching RE.")
	       (mod-apropos . "Find modules matching RE.")
	       (file-apropos . "Find files matching RE.")

	       (var-defs . "Find all definitions of a variable.")
	       (var-uses . "Find all uses of a variable.")

	       (mod-files . "Find the file defining a module.")
	       (guess-module-file . "Guess file corresponding to module.")
	       (file-modules . "List the modules defined in a file.")))
    (destructuring-bind (name . doc) x
      (let ((lisp-name (intern (format "xref-%s" name)))
	    (pl-name (format "Devel::Xref::%s" (perl-name name))))
	(when (fboundp lisp-name) (fmakunbound lisp-name))
	(eval `(defun ,lisp-name (&rest args)
		 ,doc
		 (apply #'perl-call ,pl-name 'list-context args)))))))

(defun pscope-interactive-arg (&optional type)
  (let* ((default (case type
		    (file (or (thing-at-point 'file) (buffer-file-name)))
		    (t (thing-at-point 'symbol))))
	 (text (capitalize (symbol-name type)))
	 (choices (case type
		    (variable (xref-var-apropos))
		    (function (xref-apropos))
		    (module (xref-mod-apropos))
		    (t nil)))
	 (ret (if pscope-use-completion
		  (completing-read (format "%s [%s]: " text default)
				   choices nil nil nil 'pscope-history
				   default)
		  (read-string (format "%s [%s]: " text default)
			       nil 'pscope-history default))))
    (push ret pscope-history)
    ret))

(defun pscope-interactive-module ()
"Guess which module we should look things up in.  Prompting for a
module all the time is a PITA, but I don't think this (choosing
the current file's module) is a good alternative, either.  Best
would be to choose the module based on what we know about the
symbol at point."
  (let ((xs (xref-file-modules (buffer-file-name))))
    (if (= (length xs) 1)
	(car xs)
	nil)))

(defun pscope-maybe-echo (result)
  (when (interactive-p)
    (message "%s" result))
  result)

(defun pscope-module-find (mod)
"Find the file defining module MOD."
  (interactive (list (pscope-interactive-arg 'module)))
  (let ((fn (or (pscope-module-file mod)
		(xref-guess-module-file mod))))
    (when fn
      (message "Module %s in %s." mod fn)
      (pop-to-buffer (find-file-noselect fn)))))

(defmacro ifa (test then &rest else)
  `(let ((it ,test))
     (if it ,then ,else)))

(defun pscope-show-locations (locs)
  (when locs
    (pop-to-buffer (get-buffer-create "*pscope-places*"))
    (erase-buffer)
    (dolist (loc (sort locs (lambda (a b)
			      (or (string< (car a) (car b))
				  (and (string= (car a) (car b))
				       (< (second a) (second b)))))))
      (destructuring-bind (file line name &rest blah) loc
	(let ((str "..."))
	  (ifa (find-buffer-visiting file)
	       (with-current-buffer it
		 (ifa pscope-found-refiner
		      (funcall it line name)
		      (goto-line line))
		 (message "line for %s was %d, now %d" name line
			  (line-number-at-pos))
		 (setq line (line-number-at-pos))
		 (setq str (buffer-substring (my-bol-from (point))
					     (my-eol-from (point))))))
	  (insert (format "%s:%d:%s\n" (abbreviate-file-name file) line str)))))
    (grep-mode)
    (goto-char (point-min))))

(defun pscope-filter-by-module (x)
  "Filter to limit hits by module only."
  (when (or (not module) (string= module (fourth x)))
    (list x)))

(defun pscope-filter-by-all (x)
  "Filter to limit hits by module and file."
  (when (and (or (not module) (string= module (fourth x)))
	     (or (not file) (string= file (first x))))
    (list x)))

(defmacro define-pscope-query (name doc &optional gen test prompt)
  `(defun ,name (ident &optional module file line display-p)
     ,(concat doc "

With prefix arg, list occurences in a ``grep-mode'' buffer.
Without, place the occurrences on ``pscope-found'', so that
calling ``pscope-next'' will cycle through them.

Depending on the query, MODULE, FILE, and LINE may be used to
narrow the results, as long as doing so leaves some matches.
When called interactively, they are taken from the current
buffer.
")
     (interactive (list (pscope-interactive-arg ,(or prompt ''function))
			(pscope-interactive-module)
			(buffer-file-name)
			(line-number-at-pos (point))
			current-prefix-arg
			))
     (let ((ret
	    ,(if test
		 `(let ((tmp  (,gen ident module file line)))
		    (or (mapcan #',test tmp) tmp))
		 `(,gen ident module file line))))
       ;; Always clear out the last found ring, because it's confusing
       ;; otherwise.
       (pscope-set-found nil ',(or prompt 'function))
       (if display-p
	   (pscope-show-locations ret)
	   (pscope-set-found ret ',(or prompt 'function))
	   (pscope-next)))))

(define-pscope-query pscope-defs
    "Find all definitions of sub."
  xref-defs)

(define-pscope-query pscope-uses
    "Find all uses of sub (i.e. positions within its callers)."
  xref-callers
  (lambda (x) (setf (third x) ident) (list x)))

(define-pscope-query pscope-callers
    "Find callers of FUNC."
  xref-callers)

(define-pscope-query pscope-callees
    "Find a sub's callees."
  xref-callees)

(define-pscope-query pscope-var-defs
    "Find a var's definitions."
  xref-var-defs
  (lambda (x) (setf (third x) ident) (list x))
  'variable)

(define-pscope-query pscope-var-uses
    "Find a var's uses."
  xref-var-uses
  (lambda (x) (setf (third x) ident) (list x))
  'variable)

(define-pscope-query pscope-module-describe
    "Find all subroutines in a package."
  xref-package-subs
  nil
  'module)

(defalias 'pscope-package-defs 'pscope-module-describe)

(define-pscope-query pscope-apropos
    "Find/list subroutines matching regexp."
  xref-apropos
  xref-defs
  'function)

(define-pscope-query pscope-var-apropos
    "Find/list variables matching regexp."
  xref-var-apropos
  xref-var-defs
  'variable)

(defun pscope-rebuild ()
  "Rebuild the Xref database."
  (interactive)
  (xref-rebuild))

(defvar w3m-perldoc-history nil)
(defun w3m-perldoc-this (thing)
  "View perldoc for module at point."
  (interactive (list (pscope-interactive-arg 'module)))
  (w3m-perldoc thing))

(defvar pscope-found)
(defvar pscope-found-head)
(defvar pscope-found-refiner)
(defvar pscope-history nil)

(defun pscope-set-found (list &optional type)
  (setq pscope-found list
	pscope-found-head list)
  (setq pscope-found-refiner (pscope-refiner type))
  (when (length list)
    (message "pscope: found %d %s%s." (length list)
	       (or type "item")
	       (if (= (length list) 1) "" "s"))))

(defun pscope-refiner (type)
  (case type
    (function
     (lambda (line ident)
      (let ((sub-re (concat "^\\s *sub\\s +.*" ident)))
	(cond
	  (line (goto-line line)
		(re-search-backward sub-re nil t))
	  (t (goto-char (point-min))
	     (re-search-forward sub-re nil t))))))
    (variable
     (lambda (line ident)
       (cond
	 (line (goto-line line))
	 (t (goto-char (point-min))
	    (re-search-forward (concat "\\<" ident "\\>" nil t))))))
    (t (lambda (line ident) (and line (goto-line line))))))

(defun pscope-next ()
"Go to the next thing (e.g. def, use) found by pscope."
  (interactive)
  (if pscope-found
      (destructuring-bind (file line short &optional mod &rest blah)
	  (car pscope-found)
	(unless file
	  (setq file (and mod (pscope-module-file mod)))
	  (if file
	      (setf (caar pscope-found) file)
	      (error "No file for %s." (car pscope-found))))
	(message "%s at %s:%s" short file line)
	(find-file (or file (car (xref-mod-files mod))))
	(when pscope-found-refiner
	  (funcall pscope-found-refiner line short))
	(beginning-of-line)
	(recenter)
	(setq pscope-found (or (cdr pscope-found)
			       (progn
				 (message "pscope: no more defs.")
				 pscope-found-head))))
      (message "No more definitions.")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; IPL -- interact with perl.

(defvar ipl-data-dumper-p t
"* If nil, format IPL results as Elisp expressions.  Otherwise,
convert to Perl expressions using Data::Dumper.")
(defvar ipl-data-dumper-subs-p t
"* If non-nil, have Data::Dumper try to deparse subs.")

(defun ipl-start ()
"Create a buffer to interact with a Perl interpreter.  The buffer
is placed in cperl-mode; calling ``ipl-send-line'' will evaluate
the current line and display the result."
  (interactive)
  (switch-to-buffer (get-buffer-create "*perl-interaction*"))
  (cperl-mode)
  (local-set-key "\C-j" 'ipl-send-line)
  (if ipl-data-dumper-p
      (perl-eval "use Data::Dumper; " 'void-context)))

(defun ipl-send-line (&optional scalarp)
"Send the current line to perl, and display the result."
  (interactive "P")
  (let ((str (concat "do{"
		     (buffer-substring (progn (beginning-of-line) (point))
				       (progn (end-of-line) (point)))
		     "}")))
    (insert
     (if ipl-data-dumper-p
	 (perl-eval
	  (concat "{ use Data::Dumper; local $Data::Dumper::Indent=0; "
		  (if ipl-data-dumper-subs-p
		      " local $Data::Dumper::Deparse=1; "
		      "")
		  "local $_ = Dumper("
		  (if scalarp "scalar(" "[")
		  str
		  (if scalarp ")" "]")
		  "); s/^.*?=\\s*//; s/;$//;\"\\n#=> $_\\n\"}") 'scalar-context)
       (format "\n%S\n"
	   (perl-eval str (if scalarp 'scalar-context 'list-context)))))))

(defun my-perl-frob-region (pre post beg end replace-p)
  (let* ((exp (concat pre "\""
		      (shell-quote-argument (buffer-substring beg end))
		      "\"" post))
	 (new-str (format "%s" (perl-eval exp 'scalar-context))))
    (if replace-p
	(progn (delete-region beg end)
	       (goto-char beg)
	       (insert new-str))
	(message new-str))))

(defun my-eol-from (pt)
  (save-excursion
    (goto-char pt)
    (end-of-line)
    (point)))

(defun my-bol-from (pt)
  (save-excursion
    (goto-char pt)
    (beginning-of-line)
    (point)))

(defun perl-pe-region (expr beg end &optional replace-p)
"Do the equivalent of perl -pe on region (i.e. evaluate an
expression on each line of region).  With prefix arg, replace the
region with the result."
  (interactive "MExpression:\nr\nP")
  (my-perl-frob-region
   "{ my $ret='';my $region = "
   (concat "; for (split /\n/, $region) { do { " expr
	   ";}; $ret.=\"$_\\n\"}; $ret}")
   (my-bol-from beg) (my-eol-from end) replace-p))
  
(defun perl-ize-region (expr beg end &optional replace-p)
"Evaluate a Perl expression on the region as a whole.  With
prefix arg, replace the region with the result."
  (interactive "MExpression:\nr\nP")
  (my-perl-frob-region "{ local $_ = "
		       (concat "; do { " expr ";}; $_ }")
		       beg end replace-p))

(provide 'pscope)
;;; pscope.el ends here
