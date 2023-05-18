@dev
@smoke
@regression

Feature: As a user I should be able to see the Marlowe Playground home page

    As a user I should be able to navigate to the marlowe playground home page and
    see all the starting point options available

    Scenario: As a user I expect to be able to see the available languages
      Given I am on the "home" page
      Then I should see a "link" with "Start in Javascript" text
      Then I should see a "link" with "Start in Haskell" text
      And I should see a "link" with "Start in Marlowe" text
      And I should see a "link" with "Start in Blockly" text
      And I should see a "link" with "Start in Haskell" text
      And I should see a "button" with "Open existing project" text
      And I should see a "button" with "Open an example" text

      When I click the "Open existing project" button
      Then I should see a "heading" with "Login with github" text

    Scenario Outline: As a user, I would like to see a list of example contracts by language
      Given I am on the "home" page
      When I click the "Open an example" button
      Then I should see a "heading" with "<Contract Name>" text
      And I should see a "button" with "<Contract Name> Haskell" text
      And I should see a "button" with "<Contract Name> Javascript" text
      And I should see a "button" with "<Contract Name> Marlowe" text
      And I should see a "button" with "<Contract Name> Blockly" text

    Examples:
      | Contract Name                        |
      | Escrow                               |
      | Escrow With Collateral               |
      | Zero Coupon Bond                     |
      | Coupon Bond Guaranteed               |
      | Swap                                 |
      | Contract For Differences             |
      | Contract For Differences with Oracle |
