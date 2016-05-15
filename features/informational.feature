Feature: informational

  As a user of gdid
  I want to be able to query useful information about the app

  Scenario: print version
     When I run `gdid --version`
     Then the exit status should be 0
     And the output should contain "gdid 0.0.8"
