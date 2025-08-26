;;;============================================================================

;;; File: "profile.scm"

;;; Copyright (c) 2025 by Marc Feeley, All Rights Reserved.

;;;============================================================================

;;; Generate a statistical profile for the current program.

;; See the README file for license and usage information.

(##include "~~lib/gambit/prim/prim#.scm") ;; map fx+ to ##fx+, etc
(##include "~~lib/_gambit#.scm")          ;; for macro-check-string,
                                          ;; macro-absent-obj, etc

(declare (extended-bindings)) ;; ##fx+ is bound to fixnum addition, etc
(declare (not safe))          ;; claim code has no type errors
(declare (block))             ;; claim no global is assigned

;;;============================================================================

(##add-exit-job! (lambda () (statprof-stop!) (statprof-write!)))

(statprof-start! '(profile))

;;;============================================================================
