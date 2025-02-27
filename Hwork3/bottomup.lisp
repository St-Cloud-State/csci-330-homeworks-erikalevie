;; Bottom-Up Mergesort Implementation in Lisp
;; This function sorts a list using an iterative, bottom-up approach.
;; Instead of recursively breaking the list, it starts with small sorted pairs
;; and repeatedly merges them until the entire list is sorted.


;; Merging Two Sorted Lists

;; This function merges two sorted lists into a single sorted list.
(defun merge-lists (list1 list2)
  (cond 
    ((null list1) list2)  ;; If the first list is empty, return the second.
    ((null list2) list1)  ;; If the second list is empty, return the first.
    ((<= (car list1) (car list2))  ;; Compare first elements.
     (cons (car list1) 
           (merge-lists (cdr list1) list2)))  ;; Keep the smaller one first.
    (t 
     (cons (car list2) 
           (merge-lists list1 (cdr list2))))))  ;; Otherwise, keep list2’s element.

;; Creating Sorted Pairs

;; This function groups elements into sorted pairs, which simplifies merging.
(defun make-sorted-pairs (L)
  (cond
    ((null L) nil)  ;; If the list is empty, return nil.
    ((null (cdr L)) (list (list (car L))))  ;; If there is only one element, wrap it in a list.
    (t (let ((pair (list (car L) (cadr L))))  ;; Take two elements and form a pair.
         (cons (if (<= (car L) (cadr L)) 
                   pair 
                   (reverse pair))  ;; Ensure pairs are sorted.
               (make-sorted-pairs (cddr L)))))))  ;; Move to the next two elements.


;; Merging Pairs of Sorted Lists

;; This function merges adjacent sorted pairs into progressively larger lists.
(defun merge-pass (sorted-lists)
  (cond
    ((null sorted-lists) nil)  ;; If there are no lists, return nil.
    ((null (cdr sorted-lists)) sorted-lists)  ;; If only one sorted list remains, return it.
    (t (cons (merge-lists (car sorted-lists) 
                         (cadr sorted-lists))  ;; Merge adjacent pairs.
             (merge-pass (cddr sorted-lists))))))  ;; Continue merging the rest.


;; Bottom-Up Mergesort Function

;; This function initiates the sorting process by first creating sorted pairs,
;; then continuously merging them until a single sorted list remains.
(defun bottom-up-mergesort (L)
  (cond
    ((null L) nil)  ;; If the list is empty, return nil.
    ((null (cdr L)) L)  ;; If the list has only one element, return it.
    (t (let ((sorted-pairs (make-sorted-pairs L)))  ;; Start with sorted pairs.
         (bottom-up-merge sorted-pairs)))))  ;; Continue merging until sorted.


;; Repeated Merging Until Fully Sorted

;; This function keeps merging lists until only one sorted list remains.
(defun bottom-up-merge (lists)
  (if (null (cdr lists))  ;; If there is only one list left, return it.
      (car lists)
      (bottom-up-merge (merge-pass lists))))  ;; Otherwise, continue merging.


;; Test Cases
;; Running a variety of test cases to verify correctness.

;; Sorting a shuffled list of numbers.
(print (bottom-up-mergesort '(1 7 2 1 8 6 5 3 7 9 4)))  ;; Expected: (1 1 2 3 4 5 6 7 7 8 9)

;; Edge case: Empty list.
(print (bottom-up-mergesort '()))  ;; Expected: nil

;; Edge case: Single element list.
(print (bottom-up-mergesort '(1)))  ;; Expected: (1)

;; Already sorted list.
(print (bottom-up-mergesort '(1 2 3 4 5 6 7 8)))  ;; Expected: (1 2 3 4 5 6 7 8)

;; Reverse sorted list.
(print (bottom-up-mergesort '(8 7 6 5 4 3 2 1)))  ;; Expected: (1 2 3 4 5 6 7 8)

;; Randomized numbers.
(print (bottom-up-mergesort '(8 2 7 1 3 6 4 5)))  ;; Expected: (1 2 3 4 5 6 7 8)
