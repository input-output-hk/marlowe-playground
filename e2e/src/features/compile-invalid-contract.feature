@dev
@regression
@smoke
Feature: Compile an invalid contract
    As a user I expect not to be able to compile an invalid contract
    Scenario: As a user I want to not compile invalid contracts

      Given I am on the "home" page

      When I click the "button" with "Open an example" text
      When I click the "button" with "Escrow Javascript" text
      Then I should be on the "javascript-editor" page

      When I fill in the "JS Editor" input with "invalid contract"

      When I click the "button" with "Compile" text
      Then I should see a "button" with "Compiled" text
      And the "button" with "Compiled" text should have "error" class