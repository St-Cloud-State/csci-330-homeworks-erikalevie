;; Mergesort implementation in Lisp
;; This function sorts a list using the merge sort algorithm,
;; which follows a divide-and-conquer approach.

;; Partition Function (Splitting the List)

;; This function splits the list into two nearly equal halves.
;; It does so recursively, distributing elements between two lists.

(defun partition (lst)
  (if (or (null lst) (null (cdr lst)))  ;; Base case: if list has 0 or 1 elements, return as is
      (values lst nil)
      (let ((first (car lst))   ;; First element
            (second (cadr lst)) ;; Second element
            (rest (cddr lst)))  ;; Remaining elements
        (multiple-value-bind (left right) (partition rest)
          (values (cons first left) (cons second right))))))  ;; Recursively distribute elements


;; Merge Function (Combining Two Sorted Lists)

;; This function merges two sorted lists into one sorted list.
;; It always selects the smallest element from either list first.

(defun merge (lst1 lst2)
  (cond
    ((null lst1) lst2)  ;; If lst1 is empty, return lst2
    ((null lst2) lst1)  ;; If lst2 is empty, return lst1
    ((<= (car lst1) (car lst2))
     (cons (car lst1) (merge (cdr lst1) lst2)))  ;; Keep the smaller element first
    (t
     (cons (car lst2) (merge lst1 (cdr lst2)))))) ;; Merge the remaining elements


;; Mergesort Function (Sorting the List)

;; The main mergesort function:
;; - Uses partition to split the list into two halves
;; - Recursively sorts both halves
;; - Merges them back into a sorted list

(defun mergesort (lst)
  (if (or (null lst) (null (cdr lst)))  ;; Base case: if list has 0 or 1 elements, return it
      lst
      (multiple-value-bind (left right) (partition lst)  ;; Split the list
        (merge (mergesort left) (mergesort right)))))  ;; Recursively sort and merge


;; Run a Test Case
;; This will print the sorted version of the given list.
(print (mergesort '(5 3 8 1 2 7 4 6)))