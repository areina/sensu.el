(ert-deftest sensu-event-status-text-test/with-valid-status ()
  "Should return the equivalent text of status integer value."
  (should (equal (sensu-event-status-text 1) "Warning")))
