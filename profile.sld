;;;============================================================================

;;; File: "profile.sld"

;;; Copyright (c) 2025 by Marc Feeley, All Rights Reserved.

;;;============================================================================

;;; Generate a statistical profile for the current program.

(define-library (profile)

  (import (..))  ;; relative import of statprof (preserves the version)

  (import (gambit))

  (include "profile.scm"))

;;;============================================================================
