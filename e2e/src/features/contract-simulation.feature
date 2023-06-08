@dev
@smoke
@regression
Feature: Contract Simulation

    As a user I should be able to enter currency values
    that round appropriately

    Scenario: As a user I to enter valid currency values in currency inputs

      Given I am on the "home" page

      When I click the "button" with "Open an example" text
      When I click the "button" with "Escrow Javascript" text
      Then I should be on the "javascript-editor" page

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

    Scenario: As a user who is simulating a contract, I would like to see a transaction log

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
      And I click the "button" with "Start simulation" text
      Then I should see a "heading" with "Transaction log" text
      And I should see a "heading" with "Contract started" text

      When I observe the "current" time
      And I observe the "expiration" time
      Then the "expiration" time should be greater than the "current" time

      When I click the "button" with "next minute" text
      And I observe the "current" time
      And I observe the "expiration" time
      Then I expect the "current" time to increase by "1" minutes
      And I expect the "expiration" time to increase by "0" minutes

      When I click the "button" with "next minute" text
      And I click the "button" with "next minute" text
      And I observe the "current" time
      And I observe the "expiration" time
      Then I expect the "current" time to increase by "2" minutes
      And I expect the "expiration" time to increase by "0" minutes

      When I click the "button" with "Undo" text
      And I observe the "current" time
      And I observe the "expiration" time
      Then I expect the "current" time to decrease by "1" minutes
      And I expect the "expiration" time to increase by "0" minutes

      When I click the "button" with "next timeout" text
      Then I should see a "heading" with "Contract ended" text

      When I click the "button" with "Undo" text
      And I observe the "current" time
      Then I expect the "current" time to match it's previous value

      When I click the "button" with "Add deposit" text
      And the "heading" with 'deposit transaction - (Role "Buyer")' text should contain "Buyer deposited ₳ 100 from his/her wallet into Seller account" text
      And I should see a "button" with "Everything is alright" text
      And I should see a "button" with "Report problem" text

      When I click the "button" with "Everything is alright" text
      And the "heading" with 'deposit transaction - (Role "Buyer")' text should contain "Buyer deposited ₳ 100 from his/her wallet into Seller account" text
      And the 'heading' with 'choice transaction - Everything is alright' text should contain 'Buyer choosed the value 0 for choice with id "Everything is alright"' text
      And the "heading" with 'payment transaction - (Party (Role "Seller"))' text should contain "The contract pays ₳ 100 from account of Seller to Seller" text

      When I click the "button" with "Undo" text
      Then the "heading" with 'deposit transaction - (Role "Buyer")' text should contain "Buyer deposited ₳ 100 from his/her wallet into Seller account" text
      And I should see a "button" with "Everything is alright" text
      And I should see a "button" with "Report problem" text

      When I click the "button" with "Report problem" text
      Then the "heading" with 'deposit transaction - (Role "Buyer")' text should contain "Buyer deposited ₳ 100 from his/her wallet into Seller account" text
      And the 'heading' with 'choice transaction - Report problem' text should contain 'Buyer choosed the value 1 for choice with id "Report problem"' text
      And I should see a "button" with "Confirm problem" text
      And I should see a "button" with "Dispute problem" text

      When I click the "button" with "Confirm problem" text
      Then the 'heading' with 'choice transaction - Confirm problem' text should contain 'Seller choosed the value 1 for choice with id "Confirm problem"' text
      And the 'heading' with 'payment transaction - (Party (Role "Buyer"))' text should contain 'The contract pays ₳ 100 from account of Buyer to Buyer wallet' text
      And I should see a "heading" with "Contract ended" text

      When I click the "button" with "Undo" text
      Then the "heading" with 'deposit transaction - (Role "Buyer")' text should contain "Buyer deposited ₳ 100 from his/her wallet into Seller account" text
      And the 'heading' with 'choice transaction - Report problem' text should contain 'Buyer choosed the value 1 for choice with id "Report problem"' text
      And I should see a "button" with "Confirm problem" text
      And I should see a "button" with "Dispute problem" text

      When I click the "button" with "Dispute problem" text
      # Then I should see a "heading" with "Participant Mediatorhelp_outline" text
      And the 'heading' with 'choice transaction - Dispute problem' text should contain 'Seller choosed the value 0 for choice with id "Dispute problem"' text
      And I should see a "button" with "Confirm problem" text
      And I should see a "button" with "Dismiss claim" text

      When I click the "button" with "Confirm problem" text
      Then the 'heading' with 'choice transaction - Confirm problem' text should contain 'Mediator choosed the value 1 for choice with id "Confirm problem"' text
      And the "heading" with 'payment transaction - (Party (Role "Buyer"))' text should contain "The contract pays ₳ 100 from account of Buyer to Buyer wallet" text
      And I should see a "heading" with "Contract ended" text

      When I click the "button" with "Undo" text
      And I click the "button" with "Dismiss claim" text
      Then the 'heading' with 'choice transaction - Dismiss claim' text should contain 'Mediator choosed the value 0 for choice with id "Dismiss claim"' text
      And the 'heading' with 'payment transaction - (Party (Role "Seller"))' text should contain 'The contract pays ₳ 100 from account of Buyer to Seller wallet' text
      And I should see a "heading" with "Contract ended" text
