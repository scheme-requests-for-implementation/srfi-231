(include "html-lib.scm")

(define SRFI "231") ;; must be a string

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

(define (format-global-variable name)
  (<p> (<b> "Variable: ")
       (<code> (<a> id: name name))))

(define (format-parameter name)
  (<p> (<b> "Parameter: ")
       (<code> (<a> id: name name))))

(with-output-to-file
    "commentary.html"
  (lambda()
    (html-display
     (list
      (<unprotected> "<!DOCTYPE html>")
      (<html>
       lang: 'en
       (<head>
        (<meta> charset: "utf-8")
        (<title> (string-append "Commentary on SRFI " SRFI ": Intervals and Generalized Arrays"))
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
        (<h1> "Commentary on " (<a> href: "https://srfi.schemers.org/" (<img> class: 'srfi-logo src: "https://srfi.schemers.org/srfi-logo.svg" alt: "SRFI logo")) SRFI ": Intervals and Generalized Arrays")

        (<p> " by Bradley J. Lucier")

        (<h2> id: 'status "Status")
        (<p>
         (<a> href: "https://srfi.schemers.org/srfi-231/srfi-231.html" "SRFI 231")" is currently in " (<em> "final") " status.  Here is "
         (<a> href: "https://srfi.schemers.org/srfi-process.html" "an explanation")
         " of each status that a SRFI can hold.  To provide input on this SRFI, please send email to "
         (<code> (<a> href: (string-append "mailto:srfi+minus+" SRFI "+at+srfi+dotschemers+dot+org")
                      (string-append "srfi-" SRFI "@") (<span> class: "antispam" "nospam") "srfi.schemers.org"))
         ".  To subscribe to the list, follow "
         (<a> href: "https://srfi.schemers.org/srfi-list-subscribe.html" "these instructions")
         ".  You can access previous messages via the mailing list "
         (<a> href: "https://srfi-email.schemers.org/srfi-231/" "archive")".")
        (<h2> "Some comparisons to NumPy.")
        (<p> "The "(<a> href: "https://numpy.org/doc/stable/user/index.html" "NumPy array library")" is a popular library for scientific computing that extends "(<a> href: "https://www.python.org/" "the Python programming language")". I'd like to compare some of the properties of NumPy arrays and SRFI 231 arrays.  I should state from the outset that (1) I am not in any way an expert in Python or NumPy, and (2) this commentary is not meant in any way as a criticism of NumPy, which has had tremendous success.  We refer in the following to the NumPy API Reference section on "(<a> href: "https://numpy.org/doc/stable/reference/routines.array-manipulation.html" "Array manipulation routines""."))
        (<p> "I believe that NumPy arrays are conceptually similar to SRFI 231's \"specialized arrays\", i.e., with elements stored in a one-dimensional \"vector\" of some element type accessed by addresses given by $a_0\\times i_0+\\cdots a_{n-1}\\times i_{n-1}+b$ when an array index is $i_0,\\ldots,i_{n-1}$ and $a_0,\\ldots,a_{n-1},b$ are integer constants.  SRFI 231 also has \"generalized arrays\", with a general mapping between multi-indices $i_0,\\ldots,i_{n-1}$ and values, without a backing vector.")
        (<p> "NumPy array indices begin at zero; SRFI 231 arrays have arbitrary (exact integer) lower and upper bounds, though one can specify only the upper bounds of the domain of an array and the lower bounds are assumed to be zero. Because array axes can have nonzero lower bounds, SRFI 231 contains an "(<code>'array-translate)" routine that translates the domain of valid indices.  SRFI 231 may benefit from a routine to automate translate an array's domain to set all axis lower bounds to zero.")
        (<p> "NumPy uses powerful index notation for specifying subarrays, notation along the lines of "(<code>"start:end:skip")" to specify the beginning index along an axis of a subarray, the end index, and the number of elements to skip between successive indices.  In SRFI 231  "(<code>'array-extract)" specifies subarrays, and "(<code>'array-sample)" samples elements that are uniformly spaced in an array; the latter routine requires that an array's lower bounds all be zero.  Combining "(<code>'array-extract)" and "(<code>'array-sample)" provides roughly the same functionality as NumPy's index notation.")
        (<p> "NumPy sometimes has a series of routines that do similar things, but along different axes.")
        (<p> "For example, there is this section in the API reference:")
        (<table>
         (<th> colspan: 2 "Joining arrays")
         (<tr> (<td> "concatenate([axis, out, dtype, casting])") (<td> "Join a sequence of arrays along an existing axis"))
         (<tr> (<td> "stack(arrays[, axis, out, dtype, casting])") (<td> "Join a sequence of arrays along a new axis."))
         (<tr> (<td> "block(arrays)") (<td> "Assemble an nd-array from nested lists of blocks."))
         (<tr> (<td> "vstack(tup, *[, dtype, casting])") (<td> "Stack arrays in sequence vertically (row wise)."))
         (<tr> (<td> "hstack(tup, *[, dtype, casting])") (<td> "Stack arrays in sequence horizontally (column wise)."))
         (<tr> (<td> "dstack(tup, *[, dtype, casting])") (<td> "Stack arrays in sequence depth wise (along the third axis)."))
         (<tr> (<td> "column_stack(tup)")(<td> "Stack 1-D arrays as columns into a 2-D array."))
         (<tr> (<td> "row_stack(tup, *[, dtype, casting])")(<td> "Stack arrays in sequence vertically (row wise)"))
         )
        (<p> "The fundamental NumPy routines here are "(<code>'concatenate)", where the dimension of the result is the same as the dimension of the argument arrays,  and "(<code>'stack)", where the dimension of the result is one more than the dimension of the argument arrays.  SRFI 231 has the associated routines "(<code>'array-append)" and "(<code>'array-stack)".")
        (<p> (<code>'hstack)", "(<code>'vstack)", and "(<code>'dstack)" either stack or concatenate lists of arrays, depending on dimension.  (I am not kidding.)")
        (<p> "SRFI 231 also has "(<code>'array-decurry)", which takes as an argument an array of arrays and allows simultaneous \"stacking\" of arrays, adding multiple new axes.  In this case, all the new axes are placed before the axes of the argument arrays; these new axes can be redistributed with SRFI 231's "(<code>'array-permute)". I don't see a corresponding routine in NumPy, i.e., a multi-axis \"stack\" routine.")
        (<p> "NumPy's "(<code>'block)" takes a nested list of arrays and assembles them into one large array of the same dimension, but for some reason doesn't have the same optional arguments as the other routines.  I don't know whether there are other routines that can operate on nested lists in NumPy.  SRFI 231's "(<code>'array-block)", inspired by "(<code>'numpy.block)",  takes an array of arrays as an argument.")
        (<p> "For \"splitting\" arrays, NumPy has the following routines:")
        (<table>
         (<th> colspan: 2 "Splitting arrays")
         (<tr> (<td> "split(ary, indices_or_sections[, axis])") (<td> "Split an array into multiple sub-arrays as views into ary."))
         (<tr> (<td> "array_split(ary, indices_or_sections[, axis])") (<td> "Split an array into multiple sub-arrays."))
         (<tr> (<td> "dsplit(ary, indices_or_sections)") (<td> "Split array into multiple sub-arrays along the 3rd axis (depth)."))
         (<tr> (<td> "hsplit(ary, indices_or_sections)") (<td> "Split an array into multiple sub-arrays horizontally (column-wise)."))
         (<tr> (<td> "vsplit(ary, indices_or_sections)") (<td> "Split an array into multiple sub-arrays vertically (row-wise).")))
        (<p> "As far as I can tell, all these routines split an array into parts along one axis direction, with "(<code>'split)" and "(<code>'array_split)" being the most general.")
        (<p> "By contrast, SRFI 231's "(<code>'array-tile)" can cut an array along various axes simultaneously, returning an array of subarrays; one can reconstruct the original array (translated to zero lower bounds) with "(<code>'array-block)".")
        (<p> "The NumPy routines "(<code>'*split)" are undone by NumPy's "(<code>'concatenate)".  I know of no NumPy routine that will decompose an array into sections appropriate to be reconstructed with NumPy's "(<code>'block)".")
        (<p> "NumPy has powerful syntactic notation for considering various views and slices of arrays, but I know of no NumPy routine that decomposes an array in a way that is reconstituted with the NumPy "(<code>'*stack)" routine. In contrast, one can combine SRFI 231's "(<code>'array-permute)" and "(<code>'array-curry)" to decompose an array by dimension, which is reconstituted with SRFI 231's "(<code>'array-stack)" (if along only one dimension) or "(<code>'array-decurry)" (if along several dimensions at once).")
        (<p> "NumPy has routines that it categorizes as \"Rearranging elements\":")
        (<table>
         (<th> colspan: 2 "Rearranging elements")
         (<tr> (<td> "flip(m[, axis])") (<td> "Reverse the order of elements in an array along the given axis."))
         (<tr> (<td> "fliplr(m)") (<td> "Reverse the order of elements along axis 1 (left/right)."))
         (<tr> (<td> "flipud(m)") (<td> "Reverse the order of elements along axis 0 (up/down)."))
         (<tr> (<td> "reshape(a, newshape[, order])")(<td> "Gives a new shape to an array without changing its data."))
         (<tr> (<td> "roll(a, shift[, axis])")(<td> "Roll array elements along a given axis."))
         (<tr> (<td> "rot90(m[, k, axes])") (<td> "Rotate an array by 90 degrees in the plane specified by axes.")))
        (<p> "NumPy's various \"flip\" routines are achieved by "(<code>'array-reverse)" of SRFI 231; NumPy's \"flip\" routines operate on one axis at a time, SRFI 231's "(<code>'array-reverse)" operate on all axes at once.  NumPy's "(<code>'reshape)" motivated, and is basically the same as, SRFI 231's "(<code>'specialized-array-reshape)".  SRFI 231 does not have builtin functionality mimicking NumPy's "(<code>'roll)".  NumPy's "(<code>'rot90)" routine is a special case of combining SRFI 231's "(<code>'array-reverse)" and "(<code>'array-permute)".  Indeed, by combining one call to "(<code>'array-reverse)" with one call to "(<code>'array-permute)", "(<a> href: "https://srfi-email.schemers.org/srfi-231/msg/22460166/" "SRFI 231 can generate all \"symmetries\" of a multi-dimensional hypercube")".")
        (<p> "NumPy has what it calls \"Transpose-like operations:")
        (<table>
         (<th> colspan: 2 "Transpose-like operations")
         (<tr> (<td> "moveaxis(a, source, destination)") (<td> "Move axes of an array to new positions."))
         (<tr> (<td> "rollaxis(a, axis[, start])") (<td> "Roll the specified axis backwards, until it lies in a given position."))
         (<tr> (<td> "swapaxes(a, axis1, axis2)")(<td> "Interchange two axes of an array."))
         (<tr> (<td> "ndarray.T") (<td> "View of the transposed array."))
         (<tr> (<td> "transpose(a[, axes])") (<td> "Returns an array with axes transposed.")))
        (<p> "All of NumPy's various transpose-like operations, the most general of which is "(<code>'moveaxis)", are effected in SRFI 231 with "(<code>'array-permute)".  SRFI 231 has a number of auxiliary routines that specify common useful permutations (rearrangements) of axes, or indices, as they're called here: "(<code>'index-first)", "(<code>'index-last)", "(<code>'index-rotate)", and "(<code>'index-swap)", the results of which are used as arguments to "(<code>'array-permute)".")
        (<h2> "Specifying actions and carrying out actions")
        (<p> "One philosophy of SRFI 231 is to separate the routines that specify actions and the routines that carry out actions.")
        (<p> "For example, one may find the maximum of a nonempty array "(<code>'A)" of numbers with "(<code>"(array-reduce max A)")".  One has to examine every element of "(<code>'A)" to determine the maximum.")
        (<p> "Say now you want to find the maximum of the squares of the elements of "(<code>'A)".  It's clear that you'll need to evaluate the squares of all the elements, but you don't need to store the squares all at one time in their own array with a backing store.  So when one says "(<code>"(array-reduce max (array-map square A))")", the "(<code>'array-map)" routine returns a \"generalized array\", which, in effect, extracts an element of "(<code>'A)" and squares it for each multi-index in the domain of "(<code>'A)".")
        (<p> "So SRFI 231 routines like "(map (lambda (x) (list (<code> x)", ")) '(array-copy array-assign! array-fold-left array-reduce array-every array-any))" etc., actually do something with all elements of an array, while, e.g., "(<code>'array-map)", "(<code>'array-outer-product)", and "(<code>'array-inner-product)" return generalized arrays that specify how to compute array elements later, as needed, for each multi-index, perhaps when passed to an \"action\" routine like "(<code>'array-reduce)".")
        (<h2> (<code>'specialized-array-reshape))
        (<p> "SRFI 231's "(<code>'specialized-array-reshape)" is basically the same as NumPy's "(<code>'reshape)" routine.  I'd like to give a simpler example here of what it does.")
        (<p> "Assume that you have a specialized array that you'd like to reshape into a one-dimensional specialized array.")
        (<p> "In SRFI 231, indexer functions for one-dimensional specialized arrays, which map the single argument $i$ to an index into a vector where the array element is stored, must be "(<i>'affine)", i.e., of the form $a\\times i+b$.  This implies, for example, that the array elements are stored in uniformly spaced boxes within the vector, with distance $|a|$ between boxes.")
        (<p> "And if a multi-dimensional specialized array's elements are not stored with this pattern inside the backing vector, it can't be reshaped to a one-dimensional vector.")
        (<p> "As an example, consider the array "(<code>"(define A (list->array (make-interval '#(4 4)) (iota 16)))")".  Its elements are stored consecutively in a vector of length 16, and we see: ")
        (<pre>"
(array->list* A) =>
((0 1 2 3)
 (4 5 6 7)
 (8 9 10 11)
 (12 13 14 15))")
        (<p> "We then have ")
        (<pre>"
(array->list* (array-scale A '#(1 2))) =>
((0 2)
 (4 6)
 (8 10)
 (12 14))")
        (<p> "and we see that the locations of thse elements in the backing store are ")
        (<pre>"
(0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15)
 ^     ^     ^     ^     ^     ^     ^     ^")
        (<p>"i.e., they are uniformly spaced in the backing vector, and this array can be reshaped to be one dimensional.  On the other hand, we have")
        (<pre>"
(array->list* (array-sample A '#(2 1))) =>
((0 1 2 3)
 (8 9 10 11))")
        (<p>" and the locations of these elements in the backing store are ")
        (<pre>"
(0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15)
(^  ^  ^  ^              ^  ^  ^  ^")
        (<p>"and these are "(<i>'not)" uniformly spaced in the backing store, and "(<i>'this)" array cannot be reshaped into a one-dimensional array.")
        (<p> "There are similar requirements for arrays of other dimensions.")
        ))))))
