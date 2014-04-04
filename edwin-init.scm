;;; -*- Mode: Scheme -*-

;;;; Edwin Initialization File

;;; Most of this code was written by Taylor R. Campbell and is placed
;;; in the Public Domain.  All warranties are disclaimed.  The code
;;; that was not written by Taylor R. Campbell is marked by the file it
;;; is a modification of in MIT Scheme's source.

;;; ~/.edwin-site.scm must define the variable TRC-SCHEME-DIRECTORY to
;;; be a pathname to the directory where auxiliary Edwin initialization
;;; files are stored.

(load (pathname-new-name (user-homedir-pathname)
                         ".edwin-site")
      (->environment '(EDWIN)))

(define (load-init name #!optional env)
  (load (pathname-new-name trc-scheme-directory name)
        (->environment (if (default-object? env)
                           '(EDWIN)
                           env))))

;;;; Miscellaneous Editing Customizations

;;; Yaaargh!

(define-key 'fundamental '(#\C-x #\C-c) #f)

;;; Bloody tabs.  (The buffer argument (since this is a buffer-local
;;; variable, but we want to supply #F to indicate a default value)
;;; must be an expression that is a reference to the variable FALSE,
;;; because if the literal operand is #F it will be ignored.  Silly.)

(set-variable! indent-tabs-mode #f false)

;;; Versioned files, even if Unix doesn't officially support them.

(set-variable! version-control #t)
(set-variable! vc-make-backup-files #t)

;;; Don't add spurious newlines at the end of files.

(set-variable! next-line-add-newlines #f)

;;; Support percent-sign prompts.

(set-variable! shell-prompt-pattern "^[^#$>%]*[#$%>] *")

;;; Having this false is really, really insecure.

(set-variable! inhibit-local-variables #t)

;;; Automatically enable Verilog mode in `.v' files.

(set-variable! file-type-to-major-mode
               (cons '("v" . verilog)
                     (ref-variable file-type-to-major-mode)))

(set-variable! diff-switches '("-u"))

;;; Auto-fill for Scheme and text modes.

(define (enable-auto-fill-mode! buffer)
  (enable-buffer-minor-mode! buffer (ref-mode-object auto-fill)))

(for-each (lambda (mode-hook)
            (add-event-receiver! mode-hook enable-auto-fill-mode!))
          (list (ref-variable mail-setup-hook)  ; <-- Is this needed?
                (ref-variable text-mode-hook)
                (ref-variable scheme-mode-hook)))

;;; Scheme indentation

(if (not (assoc "CALL-WITH-" (cdr scheme-mode:indent-regexps)))
    (set-cdr! scheme-mode:indent-regexps
              (cons '("CALL-WITH-" . 0)
                    (cdr scheme-mode:indent-regexps))))

(define (global-scheme-indent-method identifier method)
  (string-table-put! scheme-mode:indent-methods
                     (symbol->string identifier)
                     method))

(global-scheme-indent-method 'DEFINE-CLASS  2)
(global-scheme-indent-method 'DEFINE-METHOD 2)

;;; Paredit

(add-event-receiver! (ref-variable scheme-mode-hook)
  (lambda (buffer)
    (enable-buffer-minor-mode! buffer (ref-mode-object paredit))))

;;; Set up OUTLINE-PATTERN for Scheme modes to follow the convention
;;; in MIT Scheme's source.

(add-event-receiver! (ref-variable scheme-mode-hook)
  (lambda (buffer)
    (local-set-variable! outline-pattern
                         "^\n;;;;+"
                         buffer)))

;;;; Miscellaneous Editing Commands

;;; Edwin ought to have line-number-mode & column-number-mode.
;;; Failing that, WHAT-LINE & WHAT-COLUMN suffice; WHAT-LINE is built
;;; in, but I don't know why WHAT-COLUMN isn't.

(define-command what-column
  "Print the current column number of the point."
  ()
  (lambda ()
    (message "Column " (mark-column (current-point)))))

;;; Convenient for Scheme interaction.

(define-command what-environment
  "Print the current environment for Scheme evaluation."
  ()
  (lambda ()
    (message "Environment is "
             (let* ((environment
                     (evaluation-environment (current-buffer)))
                    (package (environment->package environment)))
               (or (and package
                        (package/name package))
                   environment)))))

(define-command expand-frame
  "Expand the frame to fit two 80-column windows in a 9x15 font."
  ()
  (lambda ()
    ((ref-command set-font) "9x15")
    ((ref-command set-frame-size) 163 40)
    ((ref-command split-window-horizontally) #f)
    ((ref-command enlarge-window-horizontally) 1)
    (set-variable! truncate-partial-width-windows #f)))

;;; This could do a better job of preserving the original state, but
;;; it probably doesn't matter much.

(define-command enable-eval-print-last-sexp
  "Set up \\[eval-last-sexp] to print output in the current buffer."
  ()
  (lambda ()
    (let ((buffer (selected-buffer)))
      (local-set-variable! evaluate-in-inferior-repl #f buffer)
      (local-set-variable! evaluation-output-receiver
                           show-evaluation-output
                           buffer))))

(define-command disable-eval-print-last-sexp
  "Stop \\[eval-last-sexp] from printing output in the buffer."
  ()
  (lambda ()
    (let ((buffer (selected-buffer)))
      (local-set-variable! evaluate-in-inferior-repl #t buffer)
      (local-set-variable! evaluation-output-receiver #f buffer))))

(define (prefix-region start end prefix)
  (do ((start start (line-start start 1)))
      ((or (not start)
           (mark> start end)))
    (insert-string prefix start)))

(define (show-evaluation-output value output-string)
  (with-output-to-transcript-buffer
   (lambda ()
     (write-string output-string)
     (transcript-write value #f)))
  (if (eq? (current-command)
           (ref-command-object eval-last-sexp))
      (let ((point (current-point)))
        (insert-newline point)
        (insert-string (transcript-value-prefix-string value
                                                       ;; No hash
                                                       #f)
                       point)
        (insert-string (transcript-value-string value)
                       point)    ; No hash number
        (if (not (string-null? output-string))
            (begin (insert-newlines 2 point)
                   (insert-string ";; Output:" point)
                   (insert-newline point)
                   (let ((start (mark-temporary-copy point)))
                     (insert-string output-string point)
                     (prefix-region start point ";")))))))

;;;; TRC's Source Header

(define-command lisp-insert-header
  "Insert TRC's Lisp source file header."
  ()
  (lambda ()
    (let* ((mode      (prompt-for-string      "Mode" #f))
           (locals    (prompt-for-local-variables))
           (title     (prompt-for-string     "Title" #f))
           (subtitle  (prompt-for-string  "Subtitle" ""
                        'DEFAULT-TYPE 'INVISIBLE-DEFAULT))
           (copyright (prompt-for-string "Copyright" ""
                        'DEFAULT-TYPE 'INVISIBLE-DEFAULT)))
      ((lambda (body)
         ;; If we were at the start of the buffer already, move the
         ;; point after the part we inserted, but otherwise preserve
         ;; the point.
         (if (group-start? (current-point))
             (body)
             (save-excursion body)))
       (lambda ()
         (set-current-point! (buffer-start (current-buffer)))
         (insert* ";;; -*- Mode: " mode)
         (if (pair? locals)
             (for-each (lambda (local)
                         (insert-string "; ")
                         (insert-string local))
                       locals))
         (insert-string " -*-")
         (insert-newlines 2)
         (insert-line ";;;; " title)
         (if (not (string-null? subtitle))
             (insert-line ";;;; " subtitle))
         (cond ((or (string=? copyright "public domain")
                    (string=? copyright "pd"))
                (insert-line public-domain-string))
               ((not (string-null? copyright))
                (insert-line (car copyright-format)
                             copyright
                             (cdr copyright-format))))
         (insert-newline)))
      (normal-mode (current-buffer)
                   ;; This says not to process the 'Edwin Variables:',
                   ;; only the -*- line.
                   #f))))

(define (prompt-for-local-variables)
  (let loop ((local-variables '()))
    (let ((local-variable (prompt-for-string "Local variable" ""
                            'DEFAULT-TYPE 'INVISIBLE-DEFAULT)))
      (if (string-null? local-variable)
          local-variables
          (loop (cons local-variable local-variables))))))

(define public-domain-string "
;;; This code is written by Taylor R. Campbell and placed in the Public
;;; Domain.  All warranties are disclaimed.")

(define copyright-format (cons "
;;; Copyright (C) " ", Taylor R. Campbell.
;;; All rights reserved.
;;; See the LICENCE file for details."))

;;;; Insertion Utilities

;;; I wonder why Edwin doesn't provide things like these built-in.

(define (insert* . items)
  (for-each (lambda (item)
              (cond ((string? item) (insert-string item))
                    ((char?   item) (insert-char   item))
                    ;; ...etc.
                    (else
                     (error:wrong-type-argument
                      item "buffer-insertable item"
                      'INSERT*))))
            items))

(define (insert-line . items)
  (apply insert* items)
  (insert-newline))

;;;; Miscellaneous Edwin Patches

(define-command eval-expression         ; edwin/evlcom.scm
  "Read and evaluate an expression in the typein window."
  "xEvaluate expression"
  (lambda (expression)
    (let* ((buffer (current-buffer))
           (environment (evaluation-environment buffer #f)))
      (cond ((ref-variable disable-evaluation-commands buffer)
             (editor-error
              "Evaluation commands disabled in this buffer."))
            ((and (ref-variable evaluate-in-inferior-repl buffer)
                  (repl-buffer-with-environment buffer environment))
             => (lambda (repl-buffer)
                  (inferior-repl-eval-expression repl-buffer
                                                 expression)))
            (else
             (if (ref-variable enable-transcript-buffer buffer)
                 (call-with-transcript-buffer
                  (lambda (buffer)
                    (insert-string
                     (fluid-let ((*unparse-with-maximum-readability?*
                                  #t))
                       (write-to-string expression))
                     (buffer-end buffer)))))
             (editor-eval buffer expression environment))))))

(define-command eval-region             ; edwin/evlcom.scm
  "Evaluate the region, printing the results in the typein window."
  "r"
  (lambda (region)
    (let* ((buffer (mark-buffer (region-start region)))
           (environment (evaluation-environment buffer #f)))
      (cond ((ref-variable disable-evaluation-commands buffer)
             (editor-error
              "Evaluation commands disabled in this buffer."))
            ((and (ref-variable evaluate-in-inferior-repl buffer)
                  (repl-buffer-with-environment buffer environment))
             => (lambda (repl-buffer)
                  (inferior-repl-eval-region repl-buffer region)))
            (else
             (evaluate-region region environment))))))

(define (repl-buffer-with-environment buffer environment)
  (let ((repl-buffer (current-repl-buffer* buffer)))
    (and repl-buffer
         ;; Use the inferior REPL only if it is in the same
         ;; environment as the buffer that we're evaluating from.
         (or (not environment)
             (eq? (->environment (ref-variable scheme-environment
                                               repl-buffer))
                  (->environment environment)))
         repl-buffer)))

;;; A convenient operation for fixing bugs in ENV.

(define (patch-eval environment expression)
  (eval expression (->environment environment)))

(define (patch identifier environment expression)
  (patch-eval environment `(SET! ,identifier ,expression)))

;;; Filter . & .. out of filename completion lists.  This should
;;; really use OS/COMPLETION-IGNORE-FILENAME?, I think.

(patch 'os/directory-list-completions   ; edwin/unix.scm
       '(edwin)
'(named-lambda (os/directory-list-completions directory prefix)
   (let ((channel (directory-channel-open directory)))
     (let loop ((results '()))
       (cond ((directory-channel-read-matching channel prefix)
              => (lambda (name)
                   (loop (if (or (string=? name ".")
                                 (string=? name ".."))
                             results
                             (cons name results)))))
             (else
              (directory-channel-close channel)
              results))))))

(patch 'os/directory-list '(edwin)      ; edwin/unix.scm
'(named-lambda (os/directory-list directory)
   (let ((channel (directory-channel-open directory)))
     (let loop ((results '()))
       (cond ((directory-channel-read channel)
              => (lambda (name)
                   (loop (if (or (string=? name ".")
                                 (string=? name ".."))
                             results
                             (cons name results)))))
             (else
              (directory-channel-close channel)
              results))))))

;;;; SNR (Scheme News Reader) Patches

(add-library-load-hook! 'NEWS-READER (lambda ()

(set-variable! news-group-author-columns 20)

(add-event-receiver! (ref-variable news-group-mode-hook)
  (let ((gmane-spam-filter
         (news-header-regexp-filter
          '(("xref" . ".*gmane\\.spam\\.detected.*")))))
    (lambda (buffer)
      (if (string-prefix? "gmane." (buffer-name buffer))
          (local-set-variable! news-header-filter
                               gmane-spam-filter
                               buffer)))))

))    ; end library load hook for NEWS-READER

;;;; IMAIL

(define-library 'IMAIL)

(define-autoload-command 'imail 'IMAIL
  "Read and edit incoming mail.")

(define-autoload-command 'imail-browser-view-container 'IMAIL
  "Browse the container of the resource being viewed in this buffer.")

(define-command imail-browser
  "Browse a mail container."
  ()
  (lambda ()
    ;; This is a horrible crock.  What's the right way to do this?
    (set! (access *command-argument*
                  (->environment '(EDWIN COMMAND-READER)))
          (cons (list 4) (key-name #\C-u)))
    (dispatch-on-command
     (ref-command-object imail-browser-view-container))))

(add-library-load-hook! 'IMAIL (lambda ()

(load-option 'IMAIL)

(site-imail-hook)

(set-variable! imail-auto-wrap #f)
(set-variable! imail-reply-with-re #t)
(set-variable! imail-summary-show-date #t)
(set-variable! imail-delete-after-output #t)

(set-variable! imail-default-dont-reply-to-names
               (apply regexp-group
                      (map re-quote-string
                           '(
                             "info-"
                             "campbell@mumble.net"
                             "campbell@bloodandcoffee.net"
                             "campbell@autodrip.bloodandcoffee.net"
                             "riastradh"
                             ))))

;;; Bug: Sending a mailbox mask of "/%" is invalid, where instead it
;;; should be "%", but IMAIL will gleefully do the wrong thing without
;;; care.  To prevent this, we add a special case.  I don't know
;;; whether this is the right thing, but it works on mumble.net at
;;; least.  Also, it has to filter out #F, because of the patch after
;;; this one to RUN-LIST-COMMAND.

(patch '%imap-mailbox-completions       ; imail/imail-imap.scm
       '(edwin imail imap-folder)
'(named-lambda (%imap-mailbox-completions prefix url)
   (let loop ((urls (run-list-command
                     url
                     (let ((mailbox
                            (imap-mailbox/url->server url prefix)))
                       (if (string=? mailbox "/")
                           "%"          ; special-case root mailbox
                           (string-append mailbox "%")))))
              (results '()))
     (if (pair? urls)
         (loop (cdr urls)
               (cond ((not (car urls))
                      results)
                     ((imap-folder-url-selectable? (car urls))
                      (cons (car urls) results))
                     ((imap-folder-url-corresponding-container
                       (car urls))
                      => (lambda (container-url)
                           (if (eq? container-url url)
                               results
                               (cons container-url results))))
                     (else results)))
         (reverse! results)))))

;;; Bug: RUN-LIST-COMMAND assumes that it will always get a folder
;;; URL.  I have no idea why.

(patch 'run-list-command                ; imail/imail-imap.scm
       '(edwin imail imap-folder)
'(named-lambda (run-list-command url mailbox)
   (let ((t (get-universal-time)))
     (map (lambda (response)
            (let ((mailbox
                   (let ((delimiter
                          (imap:response:list-delimiter response))
                         (mailbox
                          (imap:decode-mailbox-name
                           (imap:response:list-mailbox response))))
                     (if delimiter
                         (string-replace mailbox
                                         (string-ref delimiter 0)
                                         #\/)
                         mailbox)))
                  (flags (imap:response:list-flags response)))
              (let* ((url (imap-url-new-mailbox url mailbox))
                     (container? (imap-container-url? url))
                     (noselect? (memq '\\NOSELECT flags))
                     (noinferiors? (memq '\\NOINFERIORS flags)))
                (if (and noselect? noinferiors?)
                    #f
                    (let ((url
                           (if container?
                               (imap-container-url-corresponding-folder
                                url)
                               url)))
                      (set-imap-folder-url-list-time! url t)
                      (set-imap-folder-url-exists?! url #t)
                      (set-imap-folder-url-selectable?!
                       url
                       (not noselect?))
                      (if (not container?)
                          (set-imap-folder-url-corresponding-container!
                           url
                           (and (not noinferiors?)
                                (imap-url-new-mailbox
                                 url
                                 (string-append mailbox "/")))))
                      url)))))
          (with-open-imap-connection url
            (lambda (connection)
              (imap:command:list connection "" mailbox)))))))

))    ; end library load hook for IMAIL

;;; Edwin Variables:
;;; scheme-environment: '(EDWIN)
;;; End:
