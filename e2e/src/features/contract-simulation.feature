@wip
Feature: Custom text box for currency value inputs

    As a user I should be able to enter currency values
    that round appropriately

    Scenario: As a user I to enter valid currency values

      Given I am on the "home" page

      When I click the "button" with "Open an example" text
      When I click the "button" with "Escrow Javascript" text
      Then I should be on the "javascript editor" page

      When I click the "button" with "Compile" text
      Then I should see a "button" with "Compiled" text

      When I click the "button" with "Send To Simulator" text
      Then I should be on the "contract-simulation" page
      And I should see a "heading" with "Simulation has not started yet" text
      And I should see a "button" with "Start simulation" text

      When I fill in the "Currency Amount" input with "100"
      And I unblur the "Currency Amount" input
      Then the "Currency Amount" input should contain "100.000000" value

      When I fill in the "Currency Amount" input with "0.000006789"
      And I unblur the "Currency Amount" input
      Then the "Currency Amount" input should contain "0.000007" value

      When I fill in the "Currency Amount" input with "123456789012345678901234567890"
      And I unblur the "Currency Amount" input
      When I fill in the "Currency Amount" input with "123456789012345678901234567890.000000"
