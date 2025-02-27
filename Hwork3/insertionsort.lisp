;; Insertion Sort implementation in Lisp
;; This function sorts a list by inserting each element into a sorted sublist.


;; Step 1: Insert an Element Into a Sorted List
;; This function inserts an element into the correct position in a sorted list.

(defun insert-into-sorted (x sorted-list)
  (if (or (null sorted-list) (< x (car sorted-list)))  ;; If list is empty or x is smallest
      (cons x sorted-list)  ;; Insert at the front
      (cons (car sorted-list) (insert-into-sorted x (cdr sorted-list)))))  ;; Recursively find correct position


;; Step 2: Applying Insertion Sort Recursively

;; This function moves elements one by one from the unsorted list to the sorted list.

(defun insertion-sort (unsorted sorted)
  (if (null unsorted)  ;; If no more elements to sort, return sorted list
      sorted
      (insertion-sort (cdr unsorted) (insert-into-sorted (car unsorted) sorted))))  ;; Insert first element and repeat


;; Wrapper Function to Start Sorting

;; This function starts the sorting process with an empty sorted list.

(defun insertion-sort-main (lst)
  (insertion-sort lst nil))  ;; Call insertion-sort with an empty sorted list


;; Running a Test Case

;; This will print the sorted version of the given list.

(print (insertion-sort-main '(5 3 8 1 2 7 4 6))) ;; Expected output: (1 2 3 4 5 6 7 8)
