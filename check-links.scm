(define srfi-names
  (with-input-from-file "srfi-231.scm"
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
                           (set! result (cons (if (= (length obj) 3)
                                                  (cadr (caddr obj))
                                                  (car (cadadr obj)))
                                              result)))
                          ((and (not (null? obj))
                                (eq? (car obj)
                                     'format-global-variable))
                           (set! result (cons (cadadr obj) result)))
                          ((and (not (null? obj))
                                (eq? (car obj)
                                     'format-parameter))
                           (set! result (cons (cadadr obj) result)))
                          (else
                           (for-each process obj)))))
              (process obj)
              result))))))

(define links
  (with-input-from-file "srfi-231.scm"
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
                    (if (and (not (null? obj))
                             (eq? (car obj) '<a>)
                             (eq? (cadr obj) href:)
                             (string? (caddr obj))
                             (positive? (string-length (caddr obj)))
                             (eqv? (string-ref (caddr obj) 0) #\#)
                             )
                        (set! result (cons (substring (caddr obj) 1 (string-length (caddr obj)))
                                           result))
                        (for-each process obj))))
              (process obj)
              (map string->symbol result)))))))

(define (in-a-not-in-b a b)
  (do  ((a a (cdr a))
        (result '() (if (memq (car a) b)
                        result
                        (cons (car a) result))))
      ((null? a) result)))

(newline)(pp "SRFI names without links: ")
(pp (in-a-not-in-b srfi-names links))

(newline)(pp "links without srfi-names: ")
(pp (in-a-not-in-b links srfi-names))
