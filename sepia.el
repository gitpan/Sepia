;;; Sepia -- Simple Emacs-Perl InterAction: ugly, yet effective.
;;; (a.k.a. Septik -- Sean's Emacs-Perl Total Integration Kludge.)

;; Copyright (C) 2004 Sean O'Rourke.  All rights reserved, some wrongs
;; reversed.  This code is distributed under the same terms as Perl
;; itself.

;;; Commentary:

;; Primitive environment for interactive Perl development.  To use it,
;;
;;     1) Install Emacs::EPL, Data::Dumper, and Module::Info from CPAN.
;;     2) place Devel/Xref.pm somewhere in your @INC path (or edit the
;;        code below that points to it).
;;     3) put sepia.el and generic-repl.el somewhere Emacs will find them.
;;     4) In Emacs, evaluate
;;        M-x load-library <ret> sepia <ret>
;;        M-x sepia-init <ret>
;;        M-x sepia-rebuild <ret>
;;
;; This will give you an Xref database for the EPL process.  To add
;; some of your own modules, load them, then type
;;     M-x perl-eval-buffer
;;
;; To interact more closely with the Perl process, you can start up a
;; read-eval-print loop (REPL) by typing
;;     M-x generic-repl <ret> perl <ret>
;;
;; Alternatively, you can get an interactive Perl scratchpad like the
;; *scratch* buffer by typing
;;     M-x sepia-scratch <ret>
;; then type the relevant "use" statements into this buffer, and
;; evaluate them by hitting C-j.

;;; Code:

(require 'perl)
(require 'epl)
(require 'generic-repl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Xrefs -- use Perl to find definitions and uses.

(defvar sepia-use-completion t
  "* Use completion based on Xref database.")

(defvar sepia-initializer
"
BEGIN { push @INC, \"$ENV{HOME}/src/perl\" };
use Emacs::Lisp;
use Devel::Xref;
use Module::Info;
use Data::Dumper;

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

(defvar sepia-keymap
  (let ((km (make-sparse-keymap)))
    (dolist (kv '(("c" . sepia-callers)
		  ("C" . sepia-callees)
		  ("v" . sepia-var-uses)
		  ("V" . sepia-var-defs)
		  ("\M-." . sepia-dwim)
		  ("f" . sepia-defs)
		  ("r" . sepia-rebuild)
		  ("n" . sepia-next)))
      (define-key km (car kv) (cdr kv))))
  "Keymap for sepia functions.  This is just an example of how you
might want to bind your keys, which works best when bound to
`\\M-.'.  I actually bind ``sepia-next'' to `\\M-,' instead,
replacing ``tags-loop-continue'', a similar feature I never
use.")

(defun perl-name (sym)
  (substitute ?_ ?- (symbol-name sym)))

(defun sepia-init ()
"Load perl support code and start the inferior Perl process."
  (interactive)

  ;; Load perl defs:
  (perl-eval "1" 'void-context)	   ;to make sure interpreter's running
  (perl-eval sepia-initializer 'void-context)

  ;; Create glue wrappers for Module::Info funcs.
  (dolist (x '(name version inc-dir file is-core modules-used
	       packages-inside superclasses))
    (let ((name (intern (format "sepia-module-%s" x)))
	  (pl-func
	   (perl-eval (format "sub { _module_info(shift)->%s }" (perl-name x))
		      'scalar-context)))
      (when (fboundp name) (fmakunbound name))
      (eval `(defun ,name (mod)
	       (interactive (list (sepia-interactive-arg 'module)))
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

(defun sepia-interactive-arg (&optional type)
  (let* ((default (case type
		    (file (or (thing-at-point 'file) (buffer-file-name)))
		    (t (thing-at-point 'symbol))))
	 (text (capitalize (symbol-name type)))
	 (choices (case type
		    (variable (xref-var-apropos))
		    (function (xref-apropos))
		    (module (xref-mod-apropos))
		    (t nil)))
	 (ret (if sepia-use-completion
		  (completing-read (format "%s [%s]: " text default)
				   choices nil nil nil 'sepia-history
				   default)
		  (read-string (format "%s [%s]: " text default)
			       nil 'sepia-history default))))
    (push ret sepia-history)
    ret))

(defun sepia-interactive-module ()
"Guess which module we should look things up in.  Prompting for a
module all the time is a PITA, but I don't think this (choosing
the current file's module) is a good alternative, either.  Best
would be to choose the module based on what we know about the
symbol at point."
  (let ((xs (xref-file-modules (buffer-file-name))))
    (if (= (length xs) 1)
	(car xs)
	nil)))

(defun sepia-maybe-echo (result)
  (when (interactive-p)
    (message "%s" result))
  result)

(defun sepia-module-find (mod)
"Find the file defining module MOD."
  (interactive (list (sepia-interactive-arg 'module)))
  (let ((fn (or (sepia-module-file mod)
		(xref-guess-module-file mod))))
    (when fn
      (message "Module %s in %s." mod fn)
      (pop-to-buffer (find-file-noselect fn)))))

(defmacro ifa (test then &rest else)
  `(let ((it ,test))
     (if it ,then ,else)))

(defun sepia-show-locations (locs)
  (when locs
    (pop-to-buffer (get-buffer-create "*sepia-places*"))
    (erase-buffer)
    (dolist (loc (sort locs (lambda (a b)
			      (or (string< (car a) (car b))
				  (and (string= (car a) (car b))
				       (< (second a) (second b)))))))
      (destructuring-bind (file line name &rest blah) loc
	(let ((str "..."))
	  (ifa (find-buffer-visiting file)
	       (with-current-buffer it
		 (ifa sepia-found-refiner
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

(defun sepia-filter-by-module (x)
  "Filter to limit hits by module only."
  (when (or (not module) (string= module (fourth x)))
    (list x)))

(defun sepia-filter-by-all (x)
  "Filter to limit hits by module and file."
  (when (and (or (not module) (string= module (fourth x)))
	     (or (not file) (string= file (first x))))
    (list x)))

(defmacro define-sepia-query (name doc &optional gen test prompt)
  `(defun ,name (ident &optional module file line display-p)
     ,(concat doc "

With prefix arg, list occurences in a ``grep-mode'' buffer.
Without, place the occurrences on ``sepia-found'', so that
calling ``sepia-next'' will cycle through them.

Depending on the query, MODULE, FILE, and LINE may be used to
narrow the results, as long as doing so leaves some matches.
When called interactively, they are taken from the current
buffer.
")
     (interactive (list (sepia-interactive-arg ,(or prompt ''function))
			(sepia-interactive-module)
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
       (sepia-set-found nil ',(or prompt 'function))
       (if display-p
	   (sepia-show-locations ret)
	   (sepia-set-found ret ',(or prompt 'function))
	   (sepia-next)))))

(defun sepia-dwim (&optional display-p)
    "Try to DWIM:
* Find all definitions, if thing-at-point is a function
* Find all uses, if thing-at-point is a variable
* Find all definitions, if thing-at-point is a module
* Prompt otherwise
"
    (interactive "P")
    (multiple-value-bind (obj mod type) (sepia-ident-at-point)
      (if type
	  (progn
	    (sepia-set-found nil type)
	    (let ((ret (ecase type
			 (function (xref-defs obj mod))
			 (variable (xref-var-uses obj mod))
			 (module `((,(car (xref-mod-files mod)) 1 nil nil))))))
	      (if display-p
		  (sepia-show-locations ret)
		  (sepia-set-found ret type)
		  (sepia-next))))
	  (call-interactively 'sepia-defs))))

(define-sepia-query sepia-defs
    "Find all definitions of sub."
  xref-defs)

(define-sepia-query sepia-uses
    "Find all uses of sub (i.e. positions within its callers)."
  xref-callers
  (lambda (x) (setf (third x) ident) (list x)))

(define-sepia-query sepia-callers
    "Find callers of FUNC."
  xref-callers)

(define-sepia-query sepia-callees
    "Find a sub's callees."
  xref-callees)

(define-sepia-query sepia-var-defs
    "Find a var's definitions."
  xref-var-defs
  (lambda (x) (setf (third x) ident) (list x))
  'variable)

(define-sepia-query sepia-var-uses
    "Find a var's uses."
  xref-var-uses
  (lambda (x) (setf (third x) ident) (list x))
  'variable)

(define-sepia-query sepia-module-describe
    "Find all subroutines in a package."
  xref-package-subs
  nil
  'module)

(defalias 'sepia-package-defs 'sepia-module-describe)

(define-sepia-query sepia-apropos
    "Find/list subroutines matching regexp."
  xref-apropos
  xref-defs
  'function)

(define-sepia-query sepia-var-apropos
    "Find/list variables matching regexp."
  xref-var-apropos
  xref-var-defs
  'variable)

(defun sepia-rebuild ()
  "Rebuild the Xref database."
  (interactive)
  (xref-rebuild))

(defun sepia-load-file (file rebuild-p)
  "Reload a file, possibly rebuilding the Xref database.  When
called interactively, reloads the current buffer's file, and
rebuilds the database unless a prefix argument is given."
  (interactive (list (buffer-file-name) (not prefix-arg)))
  (perl-load-file file)
  (if rebuild-p
      (xref-rebuild)))

(defvar sepia-found)
(defvar sepia-found-head)
(defvar sepia-found-refiner)
(defvar sepia-history nil)

(defun sepia-set-found (list &optional type)
  (setq list
	(remove-if (lambda (x)
		     (and (not (car x)) (string= (fourth x) "main")))
		   list))
  (setq sepia-found list
	sepia-found-head list)
  (setq sepia-found-refiner (sepia-refiner type))
  (when (length list)
    (message "sepia: found %d %s%s." (length list)
	       (or type "item")
	       (if (= (length list) 1) "" "s"))))

(defun sepia-refiner (type)
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

(defun sepia-next ()
"Go to the next thing (e.g. def, use) found by sepia."
  (interactive)
  (if sepia-found
      (destructuring-bind (file line short &optional mod &rest blah)
	  (car sepia-found)
	(unless file
	  (setq file (and mod (sepia-module-file mod)))
	  (if file
	      (setf (caar sepia-found) file)
	      (error "No file for %s." (car sepia-found))))
	(message "%s at %s:%s" short file line)
	(find-file (or file (car (xref-mod-files mod))))
	(when sepia-found-refiner
	  (funcall sepia-found-refiner line short))
	(beginning-of-line)
	(recenter)
	(setq sepia-found (or (cdr sepia-found)
			       (progn
				 (message "sepia: no more defs.")
				 sepia-found-head))))
      (message "No more definitions.")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Completion

(defun sepia-ident-at-point ()
  "Find the perl identifier at point, returning
\(values object module type), where type is either 'variable,
'function, or 'module."
  (let ((cperl-under-as-char nil)
	(case-fold-search nil))
    (condition-case c
	(destructuring-bind (sbeg . send) (bounds-of-thing-at-point 'symbol)
	  (destructuring-bind (wbeg . wend) (bounds-of-thing-at-point 'word)
	    (if (member (char-after wbeg) '(?@ ?$ ?%))
		(incf wbeg))
	    (if (member (char-after sbeg) '(?@ ?$ ?%))
		(incf sbeg))
	    (if (save-excursion (goto-char wbeg)
				(looking-at "[A-Z]"))
		(values nil (buffer-substring sbeg send) 'module)
		(values (buffer-substring wbeg wend)
			(if (= sbeg wbeg) nil
			    (buffer-substring sbeg (- wbeg 2)))
			(if (member (char-before sbeg) '(?@ ?$ ?%))
			    'variable
			    'function)))))
      (wrong-number-of-arguments (values nil nil nil)))))

(defun sepia-complete-symbol ()
"Try to complete the word at point:
    * as a global variable, if it has a sigil (sorry, no lexical
      var completion).
    * as a module, if its last namepart begins with an uppercase
      letter.
    * as a function, otherwise.
The function currently ignores module qualifiers, which may be
annoying in larger programs.

The function is intended to be bound to \\M-TAB, like
``lisp-complete-symbol''."
  (interactive)
  (multiple-value-bind (obj mod type) (sepia-ident-at-point)
    (when (and type (not (and obj (string= obj ""))))
      (let ((completions
	     (ecase type
	       (function (xref-apropos (concat "^" obj mod)))
	       (variable (xref-var-apropos (concat "^" obj mod)))
	       (module (xref-mod-apropos (concat "^" mod)))))
	    (eow (end-of-thing 'word)))
	(case (length completions)
	  (0 (message "No completions for %s." (or obj mod)))
	  (1 (delete-region
	      (- eow (length (if (eq type 'module) mod obj)))
	      eow)
	     (insert (car completions)))
	  (t (delete-region
	      (- eow (length (if (eq type 'module) mod obj)))
	      eow)
	     (insert (try-completion (if (eq type 'module) mod obj)
				     completions))
	     (with-output-to-temp-buffer "*Completions*"
	       (display-completion-list completions))))))))

(defun sepia-indent-or-complete ()
"Indent the current line and, if indentation doesn't move point,
complete the symbol around point.  This function is intended to
be bound to TAB."
  (interactive)
  (let ((pos (point)))
    (cperl-indent-command)
    (when (= pos (point))
      (sepia-complete-symbol))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; scratchpad code

(defun sepia-scratchpad ()
"Create a buffer to interact with a Perl interpreter.  The buffer
is placed in cperl-mode; calling ``sepia-scratch-send-line'' will
evaluate the current line and display the result."
  (interactive)
  (switch-to-buffer (get-buffer-create "*perl-interaction*"))
  (cperl-mode)
  (local-set-key "\C-j" 'sepia-scratch-send-line))

(defun sepia-scratch-send-line (&optional scalarp)
"Send the current line to perl, and display the result."
  (interactive "P")
  (insert
   (sepia-eval (concat "do{"
		       (buffer-substring (my-bol-from (point))
					 (my-eol-from (point)))
		       "}") scalarp t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Miscellany

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

(defvar w3m-perldoc-history nil)
(defun w3m-perldoc-this (thing)
  "View perldoc for module at point."
  (interactive (list (sepia-interactive-arg 'module)))
  (w3m-perldoc thing))

(defvar sepia-eval-package "main"
"Package in which ``sepia-eval'' evaluates perl expressions.")

(defun sepia-set-eval-package (new-package)
  (setq sepia-eval-package new-package))

(defun sepia-get-eval-package ()
  sepia-eval-package)

(defun sepia-eval (string)
  "Evaluate STRING as Perl code, returning the pretty-printed
value of the last expression.  XXX: this is the only function
that requires EPL (the rest can use Pmacs)."
  (epl-eval (epl-init) nil 'scalar-context
"{ package " (or sepia-eval-package "main") ";
require Data::Dumper;
local $Data::Dumper::Indent=0; local $Data::Dumper::Deparse=1;
local $_ = Data::Dumper::Dumper([do { " string "}]);
s/^.*?=\\s*\\[//; s/\\];$//;$_}"))

(defun sepia-interact ()
  (interactive)
  (generic-repl "perl"))

(unless (assoc "perl" repl-supported-modes)
  (push '("perl" . (:map cperl-mode-map
		    :eval sepia-eval
		    :complete sepia-complete-symbol
		    :init (lambda () (local-unset-key ":"))
		    :comment-start "#"
		    :get-package sepia-get-eval-package
		    :set-package sepia-set-eval-package))
	repl-supported-modes))

(provide 'sepia)
;;; sepia.el ends here
