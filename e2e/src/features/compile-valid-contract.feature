Feature: Compile a valid contract

    As a user I expect to be able to compile a valid contract and see the generated code
    Scenario: As a user I want to compile a valid contract and see the generated code

      Given I am on the "home" page

      When I click the "button" with "Open an example" text
      When I click the "button" with "Escrow Javascript" text
      Then I should be on the "javascript editor" page

      When I click the "button" with "Compile" text
      Then I should see a "button" with "Compiled" text
      # And I should see a "link" with "Generated code" text within the "javascript-editor-container" "heading"

      # When I click the "link" with "Generated code" text
      # Then I should see a "heading" with "Generated code content" text