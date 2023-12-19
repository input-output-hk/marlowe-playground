@dev
@smoke
@regression
Feature: Reset Simulation

    As a user I want to reset the simulation when I need to

    Scenario: As a user I to reset the simulation after interacting with it

      Given I use alice lace browser
      Given I am on the "home" page

      When I click the "button" with "Open an example" text
      When I click the "button" with "Escrow Javascript" text
      Then I should be on the "javascript-editor" page

      When I click the "button" with "Compile" text
      Then I should see a "button" with "Compiled" text

      When I click the "button" with "Send To Simulator" text
      Then I should be on the "contract-simulation" page
      And I should see a "heading" with "Simulation has not started yet" text

      When I fill in the "Currency Amount" input with "100"
      And I unblur the "Currency Amount" input
      Then the "Currency Amount" input should contain "100.000000" value

      When I click the "button" with "Start simulation" text
      And I should see a "heading" with "Actions" text

      When I click the "button" with "Add deposit" text
      And I should see a "button" with "Everything is alright" text

      When I click the "button" with "Reset" text
      Then I should see a "heading" with "Simulation has not started yet" text
      And I should see a "button" with "Start simulation" text