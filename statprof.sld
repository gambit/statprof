;;;============================================================================

;;; File: "statprof.sld"

;;; Copyright (c) 2005 by Guillaume Germain, All Rights Reserved.
;;; Copyright (c) 2020 by Marc Feeley, All Rights Reserved.

;;;============================================================================

;;; Statistical profiling library

(define-library (statprof)

  (export statprof-start!
          statprof-stop!
          statprof-write!)

  (import (gambit))

  (include "statprof.scm"))

;;;============================================================================
