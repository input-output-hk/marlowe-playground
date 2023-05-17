Feature: As a user I should be able to see the Marlowe Playground home page

    As a user I should be able to navigate to the marlowe playground home page and
    see all the starting point options available

    @smoke
    @regression
    Scenario: As a user I expect to be able to see the available languages

      Given I am on the "home" page
      Then I should see "Start in Javascript" text
      And I should see "Start in Haskell" text
      And I should see "Start in Marlowe" text
      And I should see "Start in Blockly" text
      And I should see "Start in Haskell" text
      And I should see a button with "Open existing project" text
      And I should see a button with "Open an example" text

      When I click the "Open existing project" button
      Then I should see "Login with github" text

    @dev
    Scenario Outline: As a user, I would like to see a list of example contracts by language
      Given I am on the "home" page
      When I click the "Open an example" button
      Then I should see "<Contract Name> header" text
      And I should see "<Contract Name> Haskell" text
      And I should see "<Contract Name> Javascript" text
      And I should see "<Contract Name> Marlowe" text
      And I should see "<Contract Name> Blockly" text

    Examples:
      | Contract Name                        |
      | Escrow                               |
      | Escrow with Collateral               |
      | Zero Coupon Bond                     |
      | Coupon Bond Guaranteed               |
      | Swap                                 |
      | Contract For Differences             |
      | Contract For Differences with Oracle |
