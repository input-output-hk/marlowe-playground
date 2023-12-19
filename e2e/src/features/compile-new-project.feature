@dev
@smoke
@regression
Feature: Open a new project and comiple

    As a user I should be able to open a new project
    with a valid default contract

    Scenario: As a user I would like to open a new project with a valid default contract

      Given I use alice lace browser
      Given I am on the "home" page

      When I click the "link" with "Start in Javascript" text
      Then I should be on the "javascript-editor" page

      When I click the "button" with "Compile" text
      Then I should see a "button" with "Compiled" text
      And the "button" with "Compiled" text should have "success" class