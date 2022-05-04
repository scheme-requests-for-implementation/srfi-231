(include "html-lib.scm")

(define (format-lambda-list lst #!optional id)
  (let ((name (car lst))
        (arguments (cdr lst)))
    (<p> (<b> "Procedure: ")
         (<code> (<a> id: (if id id name) name)
                 (map (lambda (arg)
                        (list " " (cond ((symbol? arg)
                                         (<var> arg))
                                        ((keyword? arg)
                                         arg)
                                        (else
                                         arg))))
                      arguments)))))

(with-output-to-file
    "weird-domains.html"
  (lambda()
    (html-display
     (list
      (<unprotected> "<!DOCTYPE html>")
      (<html>
       lang: 'en
       (<head>
        (<meta> charset: "utf-8")
        (<title> "Empty and zero-dimensional domains")
        (<link>
         rel: "icon"
         sizes: "192x192"
         type: "image/png"
         href: "/favicon.png"
         )
        (<link> href: "https://srfi.schemers.org/srfi.css"
                rel: "stylesheet"
                type: "text/css")
        (<meta> name: 'viewport
                content: "width=device-width, initial-scale=1")
        (<script> type: "text/x-mathjax-config" "
MathJax.Hub.Config({
  tex2jax: {inlineMath: [['$','$'], ['\\\\(','\\\\)']]}
});")
        (<script> crossorigin: "anonymous"
                  integrity:"sha384-Ra6zh6uYMmH5ydwCqqMoykyf1T/+ZcnOQfFPhDrp2kI4OIxadnhsvvA2vv9A7xYv"
                  src: "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.1/MathJax.js?config=TeX-AMS-MML_HTMLorMML")
        )
       (<body>
        (<h1>  "Check-sheet of proper treatment of empty and zero-dimensional domains")

        (<h2> "New procedures")
        (format-lambda-list '(interval-empty? interval))
        (<p> "Empty: #t.")
        (<p> "Zero-dimensional: #f.")
        (<p> "Implemented: 2022-04-26.")
        (<p> "Tested: ")
        (<p> "Documented: ")
        (format-lambda-list '(array-empty? array))
        (<p> "Empty: #t")
        (<p> "Zero-dimensional: #f.")
        (<p> "Implemented: 2022-04-26")
        (<p> "Tested: ")
        (<p> "Documented: ")

        (<h2> "Existing procedures")
        (format-lambda-list '(translation? object))
        (<p> "Empty: Irrelevant.")
        (<p> "Zero-dimensional: Allow empty vectors.")
        (<p> "Implemented: already accepts empty vectors =:-).")
        (<p> "Tested: ")
        (<p> "Documented: 2022-04-29")
        (format-lambda-list '(permutation? object))
        (<p> "Empty: Irrelevant.")
        (<p> "Zero-dimensional: Allow empty vectors.")
        (<p> "Implemented: already accepts empty vectors =:-).")
        (<p> "Tested: ")
        (<p> "Documented: ")
        (format-lambda-list '(index-rotate n k))
        (<p> "Empty: Irrelevant.")
        (<p> "Zero-dimensional: Allow 0 <= k <= n, just because the formulas still work.")
        (<p> "Implemented: 2022-04-29")
        (<p> "Tested: 2022-04-29")
        (<p> "Documented: 2022-04-29")
        (format-lambda-list '(index-first n k))
        (<p> "Empty: Irrelevant.")
        (<p> "Zero-dimensional: Still an error when $n=0$, there is no index to put first.")
        (<p> "Implemented: ")
        (<p> "Tested: 2022-04-29")
        (<p> "Documented: 2022-04-29")
        (format-lambda-list '(index-last n k))
        (<p> "Empty: Irrelevant.")
        (<p> "Zero-dimensional: Still an error when $n=0$, there is no index to put first.")
        (<p> "Implemented: ")
        (<p> "Tested: 2022-04-29")
        (<p> "Documented: 2022-04-29")
        (format-lambda-list '(make-interval arg1 #!optional arg2))
        (<p> "Empty: Allow $\\ell_k=u_k$.")
        (<p> "Zero-dimensional: Allow empty vectors.")
        (<p> "Implemented: 2022-04-26")
        (<p> "Tested: 2022-04-29")
        (<p> "Documented: 2022-04-29")
        (format-lambda-list '(interval? obj))
        (<p> "Empty: No change")
        (<p> "Zero-dimensional: No change.")
        (<p> "Implemented: ")
        (<p> "Tested: ")
        (<p> "Documented: ")
        (format-lambda-list '(interval-dimension interval))
        (<p> "Empty: Irrelevant")
        (<p> "Zero-dimensional: Allow zero result.")
        (<p> "Implemented: already works")
        (<p> "Tested: ")
        (<p> "Documented: 2022-04-29")
        (format-lambda-list '(interval-lower-bound interval i))
        (<p> "Empty: No change.")
        (<p> "Zero-dimensional: Error.")
        (<p> "Implemented: already works")
        (<p> "Tested: ")
        (<p> "Documented: 2022-04-29")
        (format-lambda-list '(interval-upper-bound interval i))
        (<p> "Empty: No change.")
        (<p> "Zero-dimensional: Error.")
        (<p> "Implemented: already works")
        (<p> "Tested: ")
        (<p> "Documented: 2022-04-29")
        (format-lambda-list '(interval-width       interval i))
        (<p> "Empty: Allow zero result.")
        (<p> "Zero-dimensional: Error.")
        (<p> "Implemented: already works")
        (<p> "Tested: ")
        (<p> "Documented: 2022-04-29")
        (format-lambda-list '(interval-lower-bounds->list interval) 'interval-lower-bounds-rarrow-list)
        (<p> "Empty: No change.")
        (<p> "Zero-dimensional: No change.")
        (<p> "Implemented: already works")
        (<p> "Tested: 2022-04-29")
        (<p> "Documented: ")
        (format-lambda-list '(interval-upper-bounds->list interval) 'interval-upper-bounds-rarrow-list)
        (<p> "Empty: No change.")
        (<p> "Zero-dimensional: No change.")
        (<p> "Implemented: already works")
        (<p> "Tested: 2022-04-29")
        (<p> "Documented: ")
        (format-lambda-list '(interval-lower-bounds->vector interval) 'interval-lower-bounds-rarrow-vector)
        (<p> "Empty: No change.")
        (<p> "Zero-dimensional: No change.")
        (<p> "Implemented: already works")
        (<p> "Tested: 2022-04-29")
        (<p> "Documented: ")
        (format-lambda-list '(interval-upper-bounds->vector interval) 'interval-upper-bounds-rarrow-vector)
        (<p> "Empty: No change.")
        (<p> "Zero-dimensional: No change.")
        (<p> "Implemented: already works")
        (<p> "Tested: 2022-04-29")
        (<p> "Documented: ")
        (format-lambda-list '(interval-widths interval))
        (<p> "Empty: Allow zero result.")
        (<p> "Zero-dimensional: Allow empty vector.")
        (<p> "Implemented: already works")
        (<p> "Tested: 2022-04-29")
        (<p> "Documented: ")
        (format-lambda-list '(interval-volume interval))
        (<p> "Empty: Allow zero result.")
        (<p> "Zero-dimensional: result is 1.")
        (<p> "Implemented: already works.")
        (<p> "Tested: 2022-04-29")
        (<p> "Documented: ")
        (format-lambda-list '(interval= interval1 interval2))
        (<p> "Empty: No change.")
        (<p> "Zero-dimensional: No change.")
        (<p> "Implemented: already works")
        (<p> "Tested: 2022-04-29")
        (<p> "Documented: ")
        (format-lambda-list '(interval-subset? interval1 interval2))
        (<p> "Empty: No change.")
        (<p> "Zero-dimensional: all zero-dimensional intervals are equal, so subsets.")
        (<p> "Implemented: already works.")
        (<p> "Tested: 2022-04-29")
        (<p> "Documented: ")
        (format-lambda-list '(interval-contains-multi-index? interval index-0 index-1 ...))
        (<p> "Empty: Always #f.")
        (<p> "Zero-dimensional: I suppose if no indices, answer is #t, otherwise #f.")
        (<p> "Implemented: already works")
        (<p> "Tested: ")
        (<p> "Documented: ")
        (format-lambda-list '(interval-projections interval right-dimension))
        (<p> "Empty: No change.")
        (<p> "Zero-dimensional: Allow right-dimension to be zero or the dimension of the interval.")
        (<p> "Implemented: 2022-04-26")
        (<p> "Tested: ")
        (<p> "Documented: ")
        (format-lambda-list '(interval-for-each f interval))
        (<p> "Empty: No change (but fast track empty interval case).")
        (<p> "Zero-dimensional: call $f$ as a thunk.")
        (<p> "Implemented: 2022-04-26")
        (<p> "Tested: 2022-04-26")
        (<p> "Documented: ")
        (format-lambda-list '(interval-dilate interval lower-diffs upper-diffs))
        (<p> "Empty: Allow empty intervals.")
        (<p> "Zero-dimensional: Allow empty vectors for lower-diffs upper-diffs.")
        (<p> "Implemented: 2022-04-26")
        (<p> "Tested: ")
        (<p> "Documented: ")
        (format-lambda-list '(interval-intersect interval-1 interval-2 ...))
        (<p> "Empty: Allow empty intervals.")
        (<p> "Zero-dimensional: I suppose that if all the intervals are zero-dimensional, then return zero-dimensional interval.")
        (<p> "Implemented: 2022-04-26")
        (<p> "Tested: 2022-05-01")
        (<p> "Documented: ")
        (format-lambda-list '(interval-translate interval translation))
        (<p> "Empty: No change.")
        (<p> "Zero-dimensional: Allow empty translation.")
        (<p> "Implemented: Already works.")
        (<p> "Tested: 2022-05-01")
        (<p> "Documented: ")
        (format-lambda-list '(interval-permute interval permutation))
        (<p> "Empty: No change.")
        (<p> "Zero-dimensional: Allow empty permutations.")
        (<p> "Implemented: already works")
        (<p> "Tested: 2022-05-01")
        (<p> "Documented: ")
        (format-lambda-list '(interval-scale interval scales))
        (<p> "Empty: The only empty argument is (make-interval '#(0 0 ...)) because the lwoer bounds have to be zero..")
        (<p> "Zero-dimensional: Allow empty scales.")
        (<p> "Implemented: already works.")
        (<p> "Tested: 2022-05-01")
        (<p> "Documented: ")
        (format-lambda-list '(interval-cartesian-product interval #\. intervals))
        (<p> "Empty: No change.")
        (<p> "Zero-dimensional: Zero-dimensional arguments don't increase the dimension of the result.")
        (<p> "Implemented: already works.")
        (<p> "Tested: 2022-05-01")
        (<p> "Documented: ")
        (format-lambda-list '(make-storage-class getter setter checker maker copier length default data? data->body))
        (<p> "Empty: for *-data->body, allow zero elements.")
        (<p> "Zero-dimensional: I can't see how the storage-class stuff will change.")
        (<p> "Implemented: 2022-04-26")
        (<p> "Tested: ")
        (<p> "Documented: ")
        (format-lambda-list '(storage-class? m))
        (<p> "Empty: ")
        (<p> "Zero-dimensional: ")
        (<p> "Implemented: ")
        (<p> "Tested: ")
        (<p> "Documented: ")
        (format-lambda-list '(storage-class-getter m))
        (<p> "Empty: ")
        (<p> "Zero-dimensional: ")
        (<p> "Implemented: ")
        (<p> "Tested: ")
        (<p> "Documented: ")
        (format-lambda-list '(storage-class-setter m))
        (<p> "Empty: ")
        (<p> "Zero-dimensional: ")
        (<p> "Implemented: ")
        (<p> "Tested: ")
        (<p> "Documented: ")
        (format-lambda-list '(storage-class-checker m))
        (<p> "Empty: ")
        (<p> "Zero-dimensional: ")
        (<p> "Implemented: ")
        (<p> "Tested: ")
        (<p> "Documented: ")
        (format-lambda-list '(storage-class-maker m))
        (<p> "Empty: ")
        (<p> "Zero-dimensional: ")
        (<p> "Implemented: ")
        (<p> "Tested: ")
        (<p> "Documented: ")
        (format-lambda-list '(storage-class-copier m))
        (<p> "Empty: ")
        (<p> "Zero-dimensional: ")
        (<p> "Implemented: ")
        (<p> "Tested: ")
        (<p> "Documented: ")
        (format-lambda-list '(storage-class-length m))
        (<p> "Empty: ")
        (<p> "Zero-dimensional: ")
        (<p> "Implemented: ")
        (<p> "Tested: ")
        (<p> "Documented: ")
        (format-lambda-list '(storage-class-default m))
        (<p> "Empty: ")
        (<p> "Zero-dimensional: ")
        (<p> "Implemented: ")
        (<p> "Tested: ")
        (<p> "Documented: ")
        (format-lambda-list '(storage-class-data? m))
        (<p> "Empty: ")
        (<p> "Zero-dimensional: ")
        (<p> "Implemented: ")
        (<p> "Tested: ")
        (<p> "Documented: ")
        (format-lambda-list '(storage-class-data->body m) 'storage-class-data-rarrow-body)
        (<p> "Empty: OK, I guess.")
        (<p> "Zero-dimensional: input data is always one-dimensional, so no go.")
        (<p> "Implemented: ")
        (<p> "Tested: ")
        (<p> "Documented: ")
        (format-lambda-list '(make-array interval getter #\[ setter #\]))
        (<p> "Empty: It's up to user.")
        (<p> "Zero-dimensional: getter must have no arguments, setter must have one.")
        (<p> "Implemented: already works.")
        (<p> "Tested: ")
        (<p> "Documented: ")
        (format-lambda-list '(array? obj))
        (<p> "Empty: No change.")
        (<p> "Zero-dimensional: No change.")
        (<p> "Implemented: ")
        (<p> "Tested: ")
        (<p> "Documented: ")
        (format-lambda-list '(array-domain array))
        (<p> "Empty: No change.")
        (<p> "Zero-dimensional: No change.")
        (<p> "Implemented: ")
        (<p> "Tested: ")
        (<p> "Documented: ")
        (format-lambda-list '(array-getter array))
        (<p> "Empty: No change.")
        (<p> "Zero-dimensional: No change.")
        (<p> "Implemented: ")
        (<p> "Tested: ")
        (<p> "Documented: ")
        (format-lambda-list '(array-dimension array))
        (<p> "Empty: No change.")
        (<p> "Zero-dimensional: Answer is 0.")
        (<p> "Implemented: ")
        (<p> "Tested: ")
        (<p> "Documented: ")
        (format-lambda-list '(mutable-array? obj))
        (<p> "Empty: No change.")
        (<p> "Zero-dimensional: No change.")
        (<p> "Implemented: ")
        (<p> "Tested: ")
        (<p> "Documented: ")
        (format-lambda-list '(array-setter array))
        (<p> "Empty: No change.")
        (<p> "Zero-dimensional: No change.")
        (<p> "Implemented: ")
        (<p> "Tested: ")
        (<p> "Documented: ")
        (format-lambda-list '(make-specialized-array interval #\[ storage-class "generic-storage-class" #\] #\[ initial-value "(storage-class-default " storage-class")" #\] #\[ safe? "(specialized-array-default-safe?)" #\]))
        (<p> "Empty: I suppose all storage-class-body's must be able to make an empty body.  Also getters and setters should be defined as error even in the no-error-checking state.")
        (<p> "Zero-dimensional: Should The boxes hold only items of the specified type?  I guess so.")
        (<p> "Implemented: 2022-04-27")
        (<p> "Tested: ")
        (<p> "Documented: ")
        (format-lambda-list '(make-specialized-array-from-data data #\[ storage-class "generic-storage-class" #\] #\[ mutable? "(specialized-array-default-mutable?)" #\] #\[ safe? "(specialized-array-default-safe?)" #\]))
        (<p> "Empty: OK, I guess.")
        (<p> "Zero-dimensional: Error.  All resulting arrays are one-dimensional.")
        (<p> "Implemented: Changes to %%interval->basic-indexer, %%finish-specialized-array, add %%indexer-0, 2022-04-26.")
        (<p> "Tested: ")
        (<p> "Documented: ")
        (format-lambda-list '(specialized-array? obj))
        (<p> "Empty: No change")
        (<p> "Zero-dimensional: No change")
        (<p> "Implemented: ")
        (<p> "Tested: ")
        (<p> "Documented: ")
        (format-lambda-list '(array-storage-class array))
        (<p> "Empty: No change.")
        (<p> "Zero-dimensional: No change.")
        (<p> "Implemented: ")
        (<p> "Tested: ")
        (<p> "Documented: ")
        (format-lambda-list '(array-indexer array))
        (<p> "Empty: No change.")
        (<p> "Zero-dimensional: No change.")
        (<p> "Implemented: ")
        (<p> "Tested: ")
        (<p> "Documented: ")
        (format-lambda-list '(array-body array))
        (<p> "Empty: No change.")
        (<p> "Zero-dimensional: No change.")
        (<p> "Implemented: ")
        (<p> "Tested: ")
        (<p> "Documented: ")
        (format-lambda-list '(array-safe? array))
        (<p> "Empty: No change.")
        (<p> "Zero-dimensional: No change.")
        (<p> "Implemented: ")
        (<p> "Tested: ")
        (<p> "Documented: ")
        (format-lambda-list '(array-elements-in-order? A))
        (<p> "Empty: #t")
        (<p> "Zero-dimensional: #t")
        (<p> "Implemented: Changes to %%compute-array-elements-in-order?, 2022-04-26.")
        (<p> "Tested: 2022-05-03")
        (<p> "Documented: 2022-05-03")
        (format-lambda-list '(specialized-array-share array new-domain new-domain->old-domain))
        (<p> "Empty: I added a test that new-domain cannot have more elements than the old array.")
        (<p> "Zero-dimensional: new-domain->old-domain is a thunk returning a multi-index")
        (<p> "Implemented: 2022-04-27")
        (<p> "Tested: 2022-05-03")
        (<p> "Documented: 2022-05-03")
        (format-lambda-list '(array-copy array #\[ result-storage-class #\[ mutable? #\[ safe? #\] #\] #\]))
        (<p> "Empty: no change")
        (<p> "Zero-dimensional: The indexer is meaningless, so we special-case the copier case")
        (<p> "Implemented: 2022-04-27")
        (<p> "Tested: 2022-05-03")
        (<p> "Documented: 2022-05-03")
        (format-lambda-list '(array-curry array inner-dimension))
        (<p> "Empty: should be automatically handled.")
        (<p> "Zero-dimensional: If inner-dimension is 0, return an (imnutable) array that wraps each element in a zero-dimensional array; if inner-dimension is (array-dimension array), then wrap the entire array in a zero-dimensional array.")
        (<p> "Implemented: 2022-04-27")
        (<p> "Tested: 2022-05-03")
        (<p> "Documented: 2022-05-03")
        (format-lambda-list '(array-extract array new-domain))
        (<p> "Empty: nothing special")
        (<p> "Zero-dimensional: nothing special")
        (<p> "Implemented: 2022-05-03")
        (<p> "Tested: 2022-05-03")
        (<p> "Documented: 2022-05-03")
        (format-lambda-list '(array-tile A S))
        (<p> "Empty: FIXME")
        (<p> "Zero-dimensional: ")
        (<p> "Implemented: ")
        (<p> "Tested: ")
        (<p> "Documented: ")
        (format-lambda-list '(array-translate array translation))
        (<p> "Empty: works")
        (<p> "Zero-dimensional: works")
        (<p> "Implemented: 2022-05-03")
        (<p> "Tested: 2022-05-03")
        (<p> "Documented: 2022-05-03")
        (format-lambda-list '(array-permute array permutation))
        (<p> "Empty: no change.")
        (<p> "Zero-dimensional: the \"else\" case in setup-permuted-getters-and-setters works, so I'll leave it")
        (<p> "Implemented: 2022-04-27")
        (<p> "Tested: 2022-04-28")
        (<p> "Documented: 2022-04-28")
        (format-lambda-list '(array-reverse array #!optional flip?))
        (<p> "Empty: ")
        (<p> "Zero-dimensional: ")
        (<p> "Implemented: ")
        (<p> "Tested: ")
        (<p> "Documented: ")
        (format-lambda-list '(array-sample array scales))
        (<p> "Empty: ")
        (<p> "Zero-dimensional: ")
        (<p> "Implemented: ")
        (<p> "Tested: ")
        (<p> "Documented: ")
        (format-lambda-list '(array-outer-product op array1 array2))
        (<p> "Empty: No change")
        (<p> "Zero-dimensional: Calling getter is an error.")
        (<p> "Implemented: 2022-05-03")
        (<p> "Tested: 2022-05-03")
        (<p> "Documented: 2022-05-03")
        (format-lambda-list '(array-inner-product A f g B))
        (<p> "Empty: no error")
        (<p> "Zero-dimensional: error")
        (<p> "Implemented: 2022-05-03")
        (<p> "Tested: 2022-05-03")
        (<p> "Documented: 2022-05-03")
        (format-lambda-list '(array-map f array #\. arrays))
        (<p> "Empty: no problem")
        (<p> "Zero-dimensional: no problem")
        (<p> "Implemented: 2022-04-28")
        (<p> "Tested: 2022-04-28")
        (<p> "Documented: 2022-04-28")
        (format-lambda-list '(array-for-each f array #\. arrays))
        (<p> "Empty: does nothing")
        (<p> "Zero-dimensional: ")
        (<p> "Implemented: 2022-04-28")
        (<p> "Tested: 2022-04-28")
        (<p> "Documented: 2022-04-28")
        (format-lambda-list '(array-foldl op id array))
        (<p> "Empty: id")
        (<p> "Zero-dimensional: (op id element)")
        (<p> "Implemented: 2022-05-03")
        (<p> "Tested: 2022-05-03")
        (<p> "Documented: 2022-05-04")
        (format-lambda-list '(array-foldr op id array))
        (<p> "Empty: id")
        (<p> "Zero-dimensional: (op element id)")
        (<p> "Implemented: 2022-05-03")
        (<p> "Tested: 2022-05-03")
        (<p> "Documented: 2022-05-04")
        (format-lambda-list '(array-reduce op A))
        (<p> "Empty: error")
        (<p> "Zero-dimensional: return value")
        (<p> "Implemented: 2022-04-28")
        (<p> "Tested: 2022-04-28")
        (<p> "Documented: 2022-04-28")
        (format-lambda-list '(array-any pred array1 array2 "..."))
        (<p> "Empty: #f")
        (<p> "Zero-dimensional: apply pred to single element")
        (<p> "Implemented: 2022-04-28")
        (<p> "Tested: 2022-04-28")
        (<p> "Documented: 2022-04-28")
        (format-lambda-list '(array-every pred array1 array2 "..."))
        (<p> "Empty: #t")
        (<p> "Zero-dimensional: apply pred to single element")
        (<p> "Implemented: 2022-04-28")
        (<p> "Tested: 2022-04-28")
        (<p> "Documented: 2022-04-28")
        (format-lambda-list '(array->list array) 'array-rarrow-list)
        (<p> "Empty: empty list")
        (<p> "Zero-dimensional: single-element list")
        (<p> "Implemented: 2022-05-03")
        (<p> "Tested: 2022-05-03")
        (<p> "Documented: 2022-05-03")
        (format-lambda-list '(list->array domain l #\[ result-storage-class "generic-storage-class" #\] #\[ mutable? "(specialized-array-default-mutable?)" #\] #\[ safe? "(specialized-array-default-safe?)" #\]) 'list-rarrow-array)
        (<p> "Empty: empty list")
        (<p> "Zero-dimensional: single-element list")
        (<p> "Implemented: 2022-04-28")
        (<p> "Tested: 2022-04-28")
        (<p> "Documented: 2022-04-28")
        (format-lambda-list '(list*->array d nested-list #\[ result-storage-class "generic-storage-class" #\] #\[ mutable? "(specialized-array-default-mutable?)" #\] #\[ safe? "(specialized-array-default-safe?)" #\]) 'list*-rarrow-array)
        (<p> "Empty: see vector*->array discussion")
        (<p> "Zero-dimensional: ")
        (<p> "Implemented: 2022-04-28")
        (<p> "Tested: 2022-04-28")
        (<p> "Documented: 2022-04-28")
        (format-lambda-list '(array->list* A) 'array-rarrow-list*)
        (<p> "Empty: see array->vector* discussion")
        (<p> "Zero-dimensional: ")
        (<p> "Implemented: 2022-04-28")
        (<p> "Tested: 2022-04-28")
        (<p> "Documented: 2022-04-28")
        (format-lambda-list '(array->vector array) 'array-rarrow-vector)
        (<p> "Empty: No change")
        (<p> "Zero-dimensional: No change")
        (<p> "Implemented: 2022-05-03")
        (<p> "Tested: 2022-05-03")
        (<p> "Documented: 2022-05-03")
        (format-lambda-list '(vector->array domain v #\[ result-storage-class "generic-storage-class" #\] #\[ mutable? "(specialized-array-default-mutable?)" #\] #\[ safe? "(specialized-array-default-safe?)" #\]) 'vector-rarrow-array)
        (<p> "Empty: ")
        (<p> "Zero-dimensional: ")
        (<p> "Implemented: 2022-04-28")
        (<p> "Tested: 2022-04-28")
        (<p> "Documented: 2022-04-28")
        (format-lambda-list '(vector*->array d nested-vector #\[ result-storage-class "generic-storage-class" #\] #\[ mutable? "(specialized-array-default-mutable?)" #\] #\[ safe? "(specialized-array-default-safe?)" #\]) 'vector*-rarrow-array)
        (<p> "Empty: Complicated, see examples")
        (<p> "Zero-dimensional: takes argument and puts it into a zero-dimensional array (no vector-wrapping required)")
        (<p> "Implemented: 2022-04-28")
        (<p> "Tested: 2022-04-28")
        (<p> "Documented: 2022-04-28")
        (format-lambda-list '(array->vector* A) 'array-rarrow-vector*)
        (<p> "Empty: returns empty vector")
        (<p> "Zero-dimensional: returns sole value (not wrapped in a vector)")
        (<p> "Implemented: 2022-04-28")
        (<p> "Tested: 2022-04-28")
        (<p> "Documented: 2022-04-28")
        (format-lambda-list '(array-assign! destination source))
        (<p> "Empty: no change")
        (<p> "Zero-dimensional: works")
        (<p> "Implemented: 2022-05-03")
        (<p> "Tested: 2022-05-03")
        (<p> "Documented: ")
        (format-lambda-list '(array-stack k arrays #\[ storage-class #\[ mutable? #\[ safe? #\] #\] #\]))
        (<p> "Empty: 2022-05-03")
        (<p> "Zero-dimensional: 2022-05-03")
        (<p> "Implemented: ")
        (<p> "Tested: 2022-05-03")
        (<p> "Documented: ")
        (format-lambda-list '(array-append k arrays #\[ storage-class #\[ mutable? #\[ safe? #\] #\] #\]))
        (<p> "Empty: ")
        (<p> "Zero-dimensional: ")
        (<p> "Implemented: ")
        (<p> "Tested: ")
        (<p> "Documented: ")
        (format-lambda-list '(array-block A #\[ storage-class #\[ mutable? #\[ safe? #\] #\] #\]))
        (<p> "Empty: ")
        (<p> "Zero-dimensional: ")
        (<p> "Implemented: ")
        (<p> "Tested: ")
        (<p> "Documented: ")
        (format-lambda-list '(array-ref A i0 #\. i-tail))
        (<p> "Empty: error")
        (<p> "Zero-dimensional: return element")
        (<p> "Implemented: 2022-05-03")
        (<p> "Tested: 2022-05-03")
        (<p> "Documented: 2022-05-03")
        (format-lambda-list '(array-set! A v i0 #\. i-tail))
        (<p> "Empty: error")
        (<p> "Zero-dimensional: 2022-05-03")
        (<p> "Implemented: 2022-05-03")
        (<p> "Tested: 2022-05-03")
        (<p> "Documented: 2022-05-03")
        (format-lambda-list '(specialized-array-reshape array new-domain #\[ copy-on-failure? #f #\]))
        (<p> "Empty: ")
        (<p> "Zero-dimensional: ")
        (<p> "Implemented: ")
        (<p> "Tested: ")
        (<p> "Documented: ")
        ))))))
