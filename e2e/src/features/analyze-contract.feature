@wip
Feature: Analyze a contract

    As a user I want to analyze a contract for warnings, reachability and refunds on close
    Scenario: As a user I want to analyze a contract for warnings

      Given I am on the "home" page

      When I click the "button" with "Open an example" text
      When I click the "button" with "Escrow Javascript" text
      Then I should be on the "javascript-editor" page

      When I click the "button" with "Compile" text
      Then I should see a "button" with "Compiled" text
      And I should see a "link" with "Static Analysis" text

      When I click the "link" with "Static Analysis" text
      Then I should see a "button" with "Analyse for warnings" text
      Then I should see a "button" with "Analyse reachability" text
      Then I should see a "button" with "Analyse for refunds on Close" text

      When I click the "button" with "Analyse for warnings" text
      Then I should see a "heading" with "Warning Analysis Result: Warnings Found" text

      When I click the "button" with "Analyse reachability" text
      Then I should see a "heading" with "Reachability Analysis Result: Pass" text

      When I click the "button" with "Analyse for refunds on Close" text
      Then I should see a "heading" with "Close Refund Analysis Result: No implicit refunds" text