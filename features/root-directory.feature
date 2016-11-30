Feature: root directory
  
  As a user of gdid
  I need to specify the root directory of the data
  So that gdid knows where the different collections reside
  
  Scenario: by environment variable
    Given an empty directory "foo"
    When I run `env GDID_ROOT_DIR=. gdid -c foo list`
    Then the exit status should be 0

  Scenario: default is current directory
    Given an empty directory "foo"
    When I run `gdid -c foo list`
    Then the exit status should be 0
