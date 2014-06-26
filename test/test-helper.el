(require 'f)

(defvar sensu-test-path
  (f-dirname (f-this-file)))

(defvar sensu-code-path
  (f-parent sensu-test-path))

(require 'sensu (f-expand "sensu.el" sensu-code-path))
