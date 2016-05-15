Feature: informational

  As a user of gdid
  I want to be able to query useful information about the app

  Scenario: print version
     When I run `gdid --version`
     Then the exit status should be 0
     And the output should contain "gdid 0.0.8"

  Scenario: print short usage
    When I run `gdid`
    Then the exit status should be 1
    And the output should contain "gdid help | -h | --help"
    And the output should contain "gdid --version"

  Scenario Outline: print help
    When I run `gdid <arg>`
    Then the exit status should be 0
    And the output should contain "Options:"
    And the output should contain "-h       --help             Show this help"

    Examples:
    | arg     |
    | help    |
    | -h      |
    | --help  |
    | --hel   |
