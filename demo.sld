(define-library (demo)

  (import (gambit))

  (import (..))  ;; relative import of statprof (preserves the version)

  ;; in a typical program you would use this import clause to
  ;; get version 0.0.1 of the statprof library:
  ;;
  ;; (import (github.com/gambit/statprof @0.0.1))

  (begin

    (define (joes-challenge n m)

      (define (create-threads)

        (declare (fixnum) (not safe))

        (let ((threads (make-vector n)))
          (for-each
           (lambda (i)
             (vector-set!
              threads
              i
              (make-thread
               (lambda ()
                 (let ((output (vector-ref threads (modulo (+ i 1) n))))
                   (let loop ((j m))
                     (if (> j 0)
                         (let ((message (thread-receive)))
                           (thread-send output message)
                           (loop (- j 1))))))))))
           (iota n))
          threads))

      (let* ((point1
              (cpu-time))
             (threads
              (create-threads))
             (point2
              (cpu-time))
             (_
              (vector-for-each thread-start! threads))
             (point3
              (cpu-time)))

        (thread-send (vector-ref threads 0) 'go)
        (thread-join! (vector-ref threads (- m 1)))

        (let ((point4
               (cpu-time)))

          (println "number of threads: "
                   n)

          (println "number of messages received/sent by each thread: "
                   m)

          (println "thread creation time (per thread in nsecs): "
                   (* 1000000000. (/ (- point2 point1) n)))

          (println "thread start time (per thread in nsecs): "
                   (* 1000000000. (/ (- point3 point2) n)))

          (println "message receive/send time (per message in nsecs): "
                   (* 1000000000. (/ (- point4 point3) (* n m)))))))

    (define (ring n)
      (if (and (integer? n)
               (exact? n)
               (>= n 2)
               (<= n 1000000))
          (let ((m (quotient 10000000 n)))
            (joes-challenge n m))
          (error "invalid number of threads")))

    (profile-start!)
    (ring 10000)
    (profile-stop!)

    (let ((output-dir "statprof-demo-profile"))
      (write-profile-report output-dir)
      (println "To view the profile open " output-dir "/index.html"))

    (println "Demo source code: " (this-source-file))))
