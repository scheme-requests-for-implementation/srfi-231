(define code-names
  (with-input-from-file "generic-arrays.scm"
    (lambda ()
      (let loop ((result '())
                 (obj (read)))
        (if (##eof-object? obj)
            result
            (if (and (list? obj)
                     (not (null? obj))
                     (eq? (car obj) 'define)
                     (not (null? (cdr obj)))
                     (pair? (cadr obj))
                     (symbol? (caadr obj))
                     (< 0 (string-length (symbol->string (caadr obj))))
                     (not (eqv? (string-ref (symbol->string (caadr obj)) 0)
                                #\%)))
                (loop (cons (caadr obj) result)
                      (read))
                (loop result (read))))))))

(define other-names
  (with-input-from-file "generic-arrays.scm"
    (lambda ()
      (let loop ((result '())
                 (obj (read)))
        (if (##eof-object? obj)
            result
            (if (and (list? obj)
                     (not (null? obj))
                     (memq (car obj) '(define make-parameter))
                     (not (null? (cdr obj)))
                     (symbol? (cadr obj))
                     (< 0 (string-length (symbol->string (cadr obj))))
                     (not (eqv? (string-ref (symbol->string (cadr obj)) 0)
                                #\%)))
                (loop (cons (cadr obj) result)
                      (read))
                (loop result (read))))))))

(set! code-names (append other-names code-names))

(define sld-names
  (with-input-from-file "179.sld"
    (lambda ()
      (let* ((sld (read))
             (exports (cdaddr sld)))
        exports))))

(define srfi-names
  (with-input-from-file "srfi-179.scm"
    (lambda ()
      (let loop ((obj (read)))
        (if (not (and (list? obj)
                      (not (null? obj))
                      (eq? (car obj)
                           'with-output-to-file)))
            (loop (read))
            (let ((result '()))
              (define (process obj)
                (if (list? obj)
                    (cond ((and (not (null? obj))
                                (eq? (car obj)
                                     'format-lambda-list))
                           (set! result (cons (car (cadadr obj))
                                              result)))
                          ((and (not (null? obj))
                                (memq (car obj)
                                      '(format-global-variable format-parameter)))
                           (set! result (cons (cadadr obj)
                                              result)))
                          (else
                           (for-each process obj)))))
              (process obj)
              result))))))

(define (in-a-not-in-b a b)
  (do  ((a a (cdr a))
        (result '() (if (memq (car a) b)
                        result
                        (cons (car a) result))))
      ((null? a) result)))

(newline)(pp "SRFI names not in code: ")
(pp (in-a-not-in-b srfi-names code-names))

(newline)(pp "Code names not in SRFI: ")
(pp (in-a-not-in-b code-names srfi-names))

(newline)(pp "sld names not in SRFI: ")
(pp (in-a-not-in-b sld-names srfi-names))

(newline)(pp "SRFI names not in sld: ")
(pp (in-a-not-in-b srfi-names sld-names))
