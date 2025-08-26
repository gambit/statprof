;;;============================================================================

;;; File: "flamegraph.sld"

;;; Copyright (c) 2025 by Marc Feeley, All Rights Reserved.

;;;============================================================================

;;; Generate a flamegraph for the current program.

(define-library (flamegraph)

  (import (..))  ;; relative import of statprof (preserves the version)

  (import (gambit))

  (include "flamegraph.scm"))

;;;============================================================================
