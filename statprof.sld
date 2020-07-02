;;;============================================================================

;;; File: "statprof.sld"

;;; Copyright (c) 2005 by Guillaume Germain, All Rights Reserved.
;;; Copyright (c) 2020 by Marc Feeley, All Rights Reserved.

;;;============================================================================

;;; Statistical profiling library

(define-library (statprof)

  (export profile-start!
          profile-stop!
          write-profile-report)

  (include "statprof.scm"))

;;;============================================================================
