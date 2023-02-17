(in-package :xpc)


(eval-when (:load-toplevel :compile-toplevel :execute)
  (use-package :it.bese.fiveam))


(in-suite xpc-suite)


(test graph-traversal
  (let* ((*default-aircraft* (make-instance 'b777-aircraft :md-name :aircraft))
         (n1 (make-instance 'pdn-source :md-name :n1 :fm-parent *default-aircraft*
                            :neighs (c_? (connected-neighs :n1 :n2))))
         (n2 (make-instance 'pdn-source :md-name :n2 :fm-parent *default-aircraft*
                            :neighs (c_? (connected-neighs :n1 :n4 :n8))))
         (n3 (make-instance 'pdn-source :md-name :n3 :fm-parent *default-aircraft*
                            :neighs (c_? (connected-neighs :n1 :n4 :n5))))
         (n4 (make-instance 'pdn-source :md-name :n4 :fm-parent *default-aircraft*
                            :neighs (c_? (connected-neighs :n2 :n3 :n7))))
         (n5 (make-instance 'pdn-source :md-name :n5 :fm-parent *default-aircraft*
                            :neighs (c_? (connected-neighs :n3 :n6))))
         (n6 (make-instance 'pdn-source :md-name :n6 :fm-parent *default-aircraft*
                            :neighs (c_? (connected-neighs :n5 :n7))))
         (n7 (make-instance 'pdn-source :md-name :n7 :fm-parent *default-aircraft*
                            :neighs (c_? (connected-neighs :n4 :n6 :n9))))
         (n8 (make-instance 'pdn-source :md-name :n8 :fm-parent *default-aircraft*
                            :neighs (c_? (connected-neighs :n2))))
         (n9 (make-instance 'pdn-source :md-name :n9 :fm-parent *default-aircraft*
                            :neighs (c_? (connected-neighs :n7))))
         (n10 (make-instance 'pdn-source :md-name :n10 :fm-parent *default-aircraft*)))
    (loop for n in (list n1 n2 n3 n4 n5 n6 n7 n8 n9 n10)
          do (push n (kids *default-aircraft*)))
    (setf (graph-component-neighs n1) (list :n1 :n2))
    (setf (graph-component-neighs n2) (list :n1 :n4 :n8))
    (setf (graph-component-neighs n3) (list :n1 :n4 :n5))
    (setf (graph-component-neighs n4) (list :n2 :n3 :n7))
    (setf (graph-component-neighs n5) (list :n3 :n6))
    (setf (graph-component-neighs n6) (list :n5 :n7))
    (setf (graph-component-neighs n7) (list :n4 :n6 :n9))
    (setf (graph-component-neighs n8) (list :n2))
    (setf (graph-component-neighs n9) (list :n7))
    ;; node 10 is disconnected from the rest of the graph
    (is (eql :n10 (system-name (car (pdn-find-systems n10
                                                      :test (lambda (n) 
                                                              (eql (system-name n) :n10)))))))
    (is (eql 9 (length (pdn-find-systems n1))))
    (is (eql 2 (length (pdn-find-systems n2
                                         :test (lambda (n)
                                                 (or (eql (system-name n) :n1)
                                                     (eql (system-name n) :n4)))))))))
