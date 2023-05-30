@dev
Feature: Switching text editors

    As a user I expect to be able to switch between text editors

    Scenario: As a user I want to switch from Haskell, to Javascript, to Marlowe, to Blockly

      Given I am on the "home" page
      When I click the "link" with "Start in Haskell" text
      Then I should be on the "haskell editor" page
      And the "heading" for "project-title" should contain "New Project" text

      When I click the "link" with "New Project" text
      And I click the "button" with "JS Editor" text
      And I click the "button" with "Don't Save" text
      Then I should be on the "javascript editor" page

      When I click the "link" with "New Project" text
      And I click the "button" with "Marlowe" text
      And I click the "button" with "Don't Save" text
      Then I should be on the "marlowe editor" page

      When I click the "link" with "New Project" text
      And I click the "button" with "Blockly" text
      And I click the "button" with "Don't Save" text
      Then I should be on the "blockly editor" page