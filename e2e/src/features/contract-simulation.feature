@wip
Feature: Simulated a contract

    As a user I expect to be able to simulate a contract

    Scenario: As a user I want to simulate a contract

      Given I am on the "home" page

      When I click the "button" with "Open an example" text
      When I click the "button" with "Escrow Javascript" text
      Then I should be on the "javascript editor" page

      When I click the "button" with "Compile" text
      Then I should see a "button" with "Compiled" text

      When I click the "button" with "Send To Simulator" text
      Then I should be on the "contract-simulation" page
      # And I should see a "heading" with "SIMULATION HAS NOT STARTED YET" text
      And I should see a "button" with "Start simulation" text

      When I fill in the "Simulation Currency" input with "100"
      And I click the "button" with "Start simulation" text
      Then I should see a "button" with "Reset" text