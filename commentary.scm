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
         "This SRFI is currently in " (<em> "final") " status.  Here is "
         (<a> href: "https://srfi.schemers.org/srfi-process.html" "an explanation")
         " of each status that a SRFI can hold.  To provide input on this SRFI, please send email to "
         (<code> (<a> href: (string-append "mailto:srfi+minus+" SRFI "+at+srfi+dotschemers+dot+org")
                      (string-append "srfi-" SRFI "@") (<span> class: "antispam" "nospam") "srfi.schemers.org"))
         ".  To subscribe to the list, follow "
         (<a> href: "https://srfi.schemers.org/srfi-list-subscribe.html" "these instructions")
         ".  You can access previous messages via the mailing list "
         (<a> href: "https://srfi-email.schemers.org/srfi-231/" "archive")".")
        (<h2> "Some comparisons to NumPy.")
        (<p> "The "(<a> href: "https://numpy.org/doc/stable/user/index.html" "NumPy array library")" is a popular library for scientific computing that extends "(<a> href: "https://www.python.org/" "the Python programming language")". I'd like to compare some of the properties of NumPy arrays and SRFI 231 arrays.  I should state from the outset that (1) I am not in any an expert in Python or NumPy, and (2) this commentary is not meant in any way as a criticism of NumPy, which has had tremendous success.  We refer in the following to the NumPY API Reference section on "(<a> href: "https://numpy.org/doc/stable/reference/routines.array-manipulation.html" "Array manipulation routines"".")
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
        (<p> "What may not be clear is that "(<code>'concatenate)" and "(<code>'block)" are related, in that the dimension of the resulting array is the same as the input arrays, and all the "(<code>'*-stack)" routines are related, in that the resulting array has dimension that is one more than the dimensions of the argument arrays.")
        (<p> "SRFI 231 has "(<code>'array-stack)", which does what "(<code>'numpy.stack)" does, which covers the semantics of all the other "(<code>'*stack)" routines.")
        (<p> "SRFI 231 also has "(<code>'array-decurry)", which takes as an argument an array of arrays, allows simultaneous \"stacking\" of arrays in multiple directions.  In this case, all the new axes are placed before the axes of the argument arrays; these new axes can be redistributed with SRFI 231's "(<code>'array-permute)". I don't see a corresponding routine in NumPy, i.e., a multi-axis \"stack\" routine.")
        (<p> "NumPy's "(<code>'concatenate)" and SRFI 231's "(<code>'array-concatenate)" do pretty much the same thing.")
        (<p> "NumPy's "(<code>'block)" takes a nested list of arrays and assembles them into one large array of the same dimension, but for some reason doesn't have the same optional arguments as the other routines.  I don't know whether there are other routines that can operate on nested lists in NumPy.  SRFI 231's "(<code>'array-block)", inspired by "(<code>'numpy.block)",  takes an array of arrays as an argument.")
        (<p> "For \"splitting\" arrays, NumPy has the following routines:")
        (<table>
         (<th> colspan: 2 "Splitting arrays")
         (<tr> (<td> "split(ary, indices_or_sections[, axis])") (<td> "Split an array into multiple sub-arrays as views into ary."))
         (<tr> (<td> "array_split(ary, indices_or_sections[, axis])") (<td> "Split an array into multiple sub-arrays."))
         (<tr> (<td> "dsplit(ary, indices_or_sections)") (<td> "Split array into multiple sub-arrays along the 3rd axis (depth)."))
         (<tr> (<td> "hsplit(ary, indices_or_sections)") (<td> "Split an array into multiple sub-arrays horizontally (column-wise)."))
         (<tr> (<td> "vsplit(ary, indices_or_sections)") (<td> "Split an array into multiple sub-arrays vertically (row-wise).")))
        (<p> "All these routines from NumPy have similar functionality to SRFI 231's "(<code>'array-tile)", but some NumPy routines are limited to acting in one dimension per procedure call.")
        (<p> "The NumPy routines "(<code>'*split)" are undone by NumPy's "(<code>'concatenate)".  I know of no NumPy routine that will decompose an array into sections appropriate to be reconstructed with NumPy's "(<code>'block)".")
        (<p> "NumPy has powerful syntactic notation for considering various views and slices of arrays, but I know of no NumPy routine that decomposes an array in a way that is reconstituted with the various NumPy "(<code>'*stack)" routines. In contrast, one can combine SRFI 231's "(<code>'array-permute)" and "(<code>'array-curry)" to decompose an array by dimension, which is reconstituted with SRFI 231's "(<code>'array-stack)" (if along only one dimension) or "(<code>'array-decurry)" (if along several dimensions at once).")
        (<p> "NumPy has routines that it categorizes as \"Rearranging elements\":")
        (<table>
         (<th> colspan: 2 "Rearranging elements")
         (<tr> (<td> "flip(m[, axis])") (<td> "Reverse the order of elements in an array along the given axis."))
         (<tr> (<td> "fliplr(m)") (<td> "Reverse the order of elements along axis 1 (left/right)."))
         (<tr> (<td> "flipud(m)") (<td> "Reverse the order of elements along axis 0 (up/down)."))
         (<tr> (<td> "reshape(a, newshape[, order])")(<td> "Gives a new shape to an array without changing its data."))
         (<tr> (<td> "roll(a, shift[, axis])")(<td> "Roll array elements along a given axis."))
         (<tr> (<td> "rot90(m[, k, axes])") (<td> "Rotate an array by 90 degrees in the plane specified by axes.")))
        (<p> "NumPy's various \"flip\" routines are achieved by the more general "(<code>'array-reverse)" of SRFI 231.  NumPy's "(<code>'reshape)" motivated, and is basically the same as, SRFI 231's "(<code>'array-reshape)".  SRFI 231 does not have builtin functionality mimicking NumPy's "(<code>'roll)".  NumPy's "(<code>'rot90)" routine is a special case of combining SRFI 231's "(<code>'array-reverse)" and "(<code>'array-permute)".  Indeed, by combining one call to "(<code>'array-reverse)" with one call to "(<code>'array-permute)", "(<a> href: "https://srfi-email.schemers.org/srfi-231/msg/22460166/" "SRFI 231 can generate all \"symmetries\" of a multi-dimensional hypercube")".")
        (<p> "NumPy has what it calls \"Transpose-like operations:")
        (<table>
         (<th> colspan: 2 "Transpose-like operations")
         (<tr> (<td> "moveaxis(a, source, destination)") (<td> "Move axes of an array to new positions."))
         (<tr> (<td> "rollaxis(a, axis[, start])") (<td> "Roll the specified axis backwards, until it lies in a given position."))
         (<tr> (<td> "swapaxes(a, axis1, axis2)")(<td> "Interchange two axes of an array."))
         (<tr> (<td> "ndarray.T") (<td> "View of the transposed array."))
         (<tr> (<td> "transpose(a[, axes])") (<td> "Returns an array with axes transposed.")))
        (<p> "All of NumPy's various transpose-like operations, the most general of which is "(<code>'moveaxis)", are effected in SRFI 231 with "(<code>'array-permute)".  SRFI 231 has a number of auxiliary routines that specify common useful permutations (rearrangements) of axes, or indices, as they're called here: "(<code>'index-first)", "(<code>'index-last)", "(<code>'index-rotate)", and "(<code>'index-swap)", the results of which are used as arguments to "(<code>'array-permute)".")
        
        )))))))
