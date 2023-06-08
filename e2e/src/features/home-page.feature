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

      When I click the "button" with "Open existing project" text
      Then I should see a "heading" with "Login with github" text

    Scenario Outline: As a user, I would like to see a list of example contracts by language
      Given I am on the "home" page
      When I click the "button" with "Open an example" text
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

    Scenario: As a user, I would like to see links to a tutorial

      Given I am on the "home" page
      Then I should see a "link" with "Tutorial" text
      And I should see a "link" with "cardano.org" text
      And I should see a "link" with "iohk.io" text
      And I should see a "link" with "Discord" text
      And I should see a "link" with "ZenDesk" text

      When I click the "link" with "Tutorial" text
      Then a new browser tab should open for "Tutorial" at "https://docs.marlowe.iohk.io/tutorials" url

      When I click the "link" with "cardano.org" text
      Then a new browser tab should open for "cardano.org" at "https://cardano.org/" url

      When I click the "link" with "iohk.io" text
      Then a new browser tab should open for "iohk.io" at "https://iohk.io/" url

      When I click the "link" with "Discord" text
      Then a new browser tab should open for "Discord" at "https://discord.com/" url

      When I click the "link" with "ZenDesk" text
      Then a new browser tab should open for "ZenDesk" at "https://iohk.zendesk.com/hc/en-us/requests/new" url

