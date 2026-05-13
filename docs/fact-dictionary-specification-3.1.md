# Fact Dictionary 3.1 Informal Specification

Author: Alexander Petros

## Introduction

This document describes the existing behavior of the Fact Dictionary, as of Fact Graph 3.1.
It includes all the functionality (and then some) that is used by TWE and EITC.
Notes are also included on interfaces that I believe could be improved and functionality that I believe should be removed in a future update.
It does not describe the Java or JavaScript APIs for creating and using Fact Graphs.

Writing a specification is a pre-requisite to improving the Fact Graph in any meaningful way.
As the Fact Graph is integrated into new applications, we must document how it is supposed to work, so that undefined behavior does not become entrenched in a variety of use-cases.
Doing so will also make it easier to onboard new applications and developers to the Fact Graph—previously, developers simply pattern-matched the XML to see what was possible.

The Fact Graph library has numerous idiosyncratic interfaces than are leaky implementation details, such as:

* The types being exposed in the JSON graph i.e. `DollarWrapper`
* The fact that there is a difference between `Dollar` and `DollarWrapper`
* The [cumbersome non-interface](https://github.com/IRSDigitalService/tax-withholding-estimator/blob/52f6b33667421145f8201a65eee087202272091b/src/main/resources/twe/website-static/js/fg-components.js#L707) for inspecting types from JavaScript
* The three different ways to specify paths in `<Dependency>` which only work in certain contexts

With a specification in hand, we can clarify exactly which parts of the library are intended behavior which need to be carried forward to a new implementation.
Then parts, if not all, of the Fact Graph can be re-implemented to resolve these issues in future versions.

This specification is denoted as "informal" due to the time constraints that prevent a more formal one.

## Fact Definitions

### `<FactDictionaryModule>`

A Fact Dictionary Module contains a single child `<Facts>` element.
It has no other purpose or attributes.
It could plausibly be used for better error reporting by categorizing the module that facts belong to.

### `<Facts>`

A Facts element contains multiple `<Fact>` elements.

### `<Fact>`

Must have one of `<Writable>` or `<Derived>` as a child.
Can also have `<Description>`, `<Placeholder>`, `<TaxYear>`, and `<Override>` as children.

Defines a fact.
Writable facts take their values from user input (their fact graph), while derived facts are calculated based on other facts.

```xml
<Fact path="/totalTax">
  <Derived>
    <Add>
      <Dependency path="/tentativeTaxNetNonRefundableCredits"/>
      <Dependency path="/totalOtherTaxes"/>
    </Add>
  </Derived>
</Fact>
```

The Fact Graph support collections; each item in a collection has the same possible facts.
Collection facts have a wildcard `*` character in their path.
During evaluation, the `*` will get replaced with a `#` and the UUID for the particular item in that collection.

For example, for a job with the UUID `a3006af1-a040-4235-9d31-68c5830c55fd`, the path to that job's `jobs/*/yearToDateIncome` fact would be `/jobs/#a3006af1-a040-4235-9d31-68c5830c55fd/yearToDateIncome`.

```xml
<Fact path="/jobs/*/yearToDateIncome">
  <Writable>
    <Dollar/>
  </Writable>
</Fact>
```

### `<Derived>`

Denotes a derived fact and the calculation that the fact is derived from.
Must have a single child, which must be an expression.

```xml
<Fact path="/maxCtcAmount">
  <Derived>
    <Dollar>2200</Dollar>
  </Derived>
</Fact>
```

### `<Writable>`

Denotes a writable fact and what type of value can be written to it.
Must have a type as a child node, and can optionally have any number of `<Limit>` children.

```xml
<Fact path="/wantsStandardDeduction">
  <Writable>
    <Boolean />
  </Writable>
</Fact>
```

### `<Limit>`

A limit on a Writable fact.
Must have the `type` attribute, which can be either `Min` or `Max`.

The child of the limit must evaluate to an expression of the same type.

```xml
<Writable>
  <Dollar />
  <Limit type="Min">
    <Dollar>0</Dollar>
  </Limit>
</Writable>
```

### `<Placeholder>`

Writable facts can have placeholder values.
The fact will evaluate to the placeholder value if the fact hasn't been set by the user.
Note that the fact, and all facts that depend on it, will still be considered "incomplete" if the placeholder value is used, even though it will have a value.
For more on completeness, see [`<IsComplete>`](#iscomplete).

The child of the placeholder must evaluate to an expression of the same type.

```xml
<Fact path="/eitcQualifyingChildren">
  <Name>Number of EITC qualifying children</Name>
  <Writable>
    <Int/>
  </Writable>
  <Placeholder>
    <Int>0</Int>
  </Placeholder>
</Fact>
```

### `<Override>`

Another control for Writable facts, which allows for overriding the user's entry if a `<Condition>` is met.
`<Condition>` and `<Default>` are both required children.
`<Condition>` must evaluate to a boolean and `<Default>` must evaluate to a value of the same type as the fact.

```xml
<Fact path="/jobs/*/amountWithheldLastPaycheck">
  <Override>
    <Condition>
      <Dependency path="../isPastJob" />
    </Condition>
    <Default>
      <Dollar>0</Dollar>
    </Default>
  </Override>
  <Writable>
    <Dollar/>
  </Writable>
</Fact>
```
> [!NOTE]
> I think that `<Default>` is slightly misnamed; it should be `<Value>`.
> Better yet, the tag could be omitted entirely.


## Fact Graph Types

The following elements are valid types.
They can be used as the child value for a writable.
In this position, these elements cannot have children.

### `<Int>`

An integer.

### `<Boolean>`

A true or false boolean.

### `<Dollar>`

A dollar value. Implemented as a BigDecimal for precision reasons.

### `<Day>`

A calendar date, usually written in YYYY-MM-DD format.

Facts with a Day type can also get the integer value of their year, month, or day by adding `/year`, `/month`, `/day`, or `/ordinal` to the end of the fact.

```xml
<Dependency path="../mostRecentPayDate/day"/>
```

> [!NOTE]
> I think that this should be accomplished with a new element instead.
> The semantics of the path are quite overloaded as it is.

### `<String>`

A string of UTF-8 text.

### `<Collection>`

A collection with associated subfacts.
Collections are defined at a "root" path and have a series of subfacts after a `*` wildcard.
For instance, the `/jobs` writable collection might have a fact like `/jobs/*/income`.

### `<Enum>` and `<MultiEnum>`

An enumerated type that can be one of a defined set of values.
Those values are defined separately, in an `<EnumOptions>` fact, and referenced here with the required `optionsPath` attribute.

```xml
<Enum optionsPath="/filingStatusOptions" />
```

`<MultiEnum />` works the same way, but allows for multiple of the options to be selected at the same time.

## Value Expressions

### `<Dependency>`

Evaluates to the current value of another fact in the Fact Graph.
Requires a `path` attribute with the path of the fact that should be evaluated.

Most paths start with a `/`.

```xml
<Dependency path="/isFilingStatusMFJ" />
```

When the dependency is the child of a definition for a collection fact, a leading `../` can be used in the path to refer to another fact of the same item.

```xml
<Fact path="/jobs/*/income">
  <Derived>
    <Add>
      <Dependency path="../restOfYearIncome"/>
      <Dependency path="../yearToDateIncome"/>
      <Dependency path="../totalFutureBonus"/>
    </Add>
  </Derived>
</Fact>
```

A "list" of facts in a collection can be referred to, using the wildcard `*`.
This is also used for aggregation facts, like `<CollectionSum>`

```xml
<CollectionSum>
  <Dependency path="/jobs/*/income" />
</CollectionSum>
```

The `<Filter>` predicate also supports prefix-less (no slash) paths.
See that definition for more details.

### Type Values

Most of the types can be used *with* children as constant values.
These work in a fairly obvious way:

```xml
<Int>3</Int>
```

```xml
<Dollar>1200.50</Dollar>
```

```xml
<Boolean>True</Boolean>
```

```xml
<Day>2025-12-31</Day>
```

```xml
<String>Hello!</String>
```

### `<Enum>`

Enums can also be used as values, with the string value of the enum as the child text node.
This is slightly less obvious, as you need to also specify the `optionsPath`.

```xml
<Enum optionsPath="/filingStatusOptions">marriedFilingSeparately</Enum>
```

### `<Rational>`

A rational number.
This makes complicated multiplication and division more legible.

```xml
<Derived>
  <Round>
    <Multiply>
      <Dependency path="/edcNetCreditAmount" />
      <Rational>15/100</Rational>
    </Multiply>
  </Round>
</Derived>
```

### `<Today>`

Evaluates to today's date.

```xml
<Derived>
  <Today />
</Derived>
```

### `<LastDayOfMonth>`

Evaluates to the last day of the given day's month.
Child must evaluate to a day.

```xml
<LastDayOfMonth>
  <Dependency path="../mostRecentPayDate"/>
</LastDayOfMonth>
```


### `<Days>`

Represents the difference between two `<Day>`s.
Primarily used in date math.

```xml
<Add>
  <Day>2025-12-31</Day>
  <Days>3</Days>
</Add>
```

### `<EnumOptions>` and `<EnumOption>`

The values that can be used in an `<Enum>`.
Children must be either `<String>` or `<EnumOption>` values, and there must be at least one.

```xml
<EnumOptions>
  <String>single</String>
  <String>qualifiedSurvivingSpouse</String>
  <String>headOfHousehold</String>
  <String>marriedFilingSeparately</String>
  <String>marriedFilingJointly</String>
</EnumOptions>
```

`<EnumOption>` can be used to condition whether certain options are available.
Each `<EnumOption>` has a `<Condition>` expression which must evaluate to a boolean, and a `<Value>` which must be a string constant.
In the below example, the first two options are only valid when `/isFilingStatusMFJ` evaluates to true.

```xml
<EnumOptions>
  <String>filer</String>
  <EnumOption>
    <Condition>
      <Dependency path="/isFilingStatusMFJ" />
    </Condition>
    <Value>
      <String>spouse</String>
    </Value>
  </EnumOption>
  <EnumOption>
    <Condition>
      <Dependency path="/isFilingStatusMFJ" />
    </Condition>
    <Value>
      <String>both</String>
    </Value>
  </EnumOption>
  <String>neither</String>
</EnumOptions>
```

## Logical Expressions

### `<Not>`

Evaluates to True if the expression is False, False otherwise.
The child expression must evaluate to a boolean.

```xml
<Not>
  <True/>
</Not>
```

### `<All>`

True if all the expressions are True, False otherwise.
All child expressions must evaluate to a boolean.

```xml
<All>
  <Dependency path="../isHourlyJob" />
  <Not>
    <Dependency path="../isPastJob" />
  </Not>
</All>
```

### `<Any>`

True if any of the expressions are True, False otherwise.
All child expressions must evaluate to a boolean.

```xml
<Any>
  <Dependency path="/isFilingStatusMFJ" />
  <Dependency path="/isFilingStatusQSS" />
</Any>
```

### `<Equal>`

True if the `<Left>` expression is mathematically equal to the `<Right>` expression.
The below example evaluates to True, even though Dollar and Int are different types.

```xml
<Equal>
  <Left>
    <Dollar>3.0</Dollar>
  </Left>
  <Right>
    <Int>3</Int>
  </Right>
</Equal>
```

> [!NOTE]
> In my opinion, the `<Left>` and `<Right>` are totally superfluous here, and should be removed.
> I believe it was done this way to be symmetrical with the comparitor nodes, but I don't think that's worth it.
> Besides, it's totally reasonable to ask if three things are equal.

### `<NotEqual>`

True if the `<Left>` expression is mathematically equal to the `<Right>` expression.

```xml
<Any>
  <Left>
    <Dollar>4.0</Dollar>
  </Left>
  <Right>
    <Dollar>12000</Dollar>
  </Right>
</Any>
```

### `<LessThan>`
True if the `<Left>` expression is mathematically less than the `<Right>` expression.

```xml
<LessThan>
  <Left>
    <Dependency path="/taxGap" />
  </Left>
  <Right>
    <Dollar>0</Dollar>
  </Right>
</LessThan>
```

### `<LessThanOrEqual>`
True if the `<Left>` expression is mathematically less than or equal to the `<Right>` expression.

```xml
<LessThanOrEqual>
  <Left>
    <Dependency path="/taxGap" />
  </Left>
  <Right>
    <Dollar>0</Dollar>
  </Right>
</LessThanOrEqual>
```

### `<GreaterThan>`
True if the `<Left>` expression is mathematically greater than the `<Right>` expression.

```xml
<GreaterThan>
  <Left>
    <Dependency path="/taxGap" />
  </Left>
  <Right>
    <Dollar>0</Dollar>
  </Right>
</GreaterThan>
```

### `<GreaterThanOrEqual>`
True if the `<Left>` expression is mathematically greater than or equal to the `<Right>` expression.

```xml
<GreaterThanOrEqual>
  <Left>
    <Dependency path="/taxGap" />
  </Left>
  <Right>
    <Dollar>0</Dollar>
  </Right>
</GreaterThanOrEqual>
```

### `<IsComplete>`

True if the child expression can be evaluated with no placeholders, False otherwise.

```xml
<IsComplete>
  <Dependency path="/jobSelectedForExtraWithholding" />
</IsComplete>
```

## Mathematical Expressions

### `<GreaterOf>`

Evaluates to the mathematically highest value of all the children.

```xml
<GreaterOf>
  <Dependency path="/minimumDeductionForActiveQBIAmount" />
  <Dependency path="/tentativeQualifiedBusinessIncomeDeduction" />
</GreaterOf>
```

### `<LesserOf>`

Evaluates to the mathematically lowest value of all the children.

```xml
<LesserOf>
  <Dependency path="/studentLoanInterestAmount" />
  <Dependency path="/maxStudentLoanDeduction" />
</LesserOf>
```

### `<Add>`

Evaluates to the sum of the child elements.

```xml
<Add>
  <Dependency path="/standardDeduction" />
  <Dependency path="/hypotheticalNonItemizerCharitableContributionDeduction" />
</Add>
```

### `<Subtract>`

Evaluates to the difference between the `<Minuend>` and the sum of all the `<Subtrahends>`.

```xml
<Subtract>
  <Minuend>
    <Dependency path="../fullPayPeriods" />
  </Minuend>
  <Subtrahends>
    <Dependency path="../payPeriodsBeforeW4ChangesAppear" />
  </Subtrahends>
</Subtract>
```

### `<Multiply>`

Evaluates to the product of all the child terms.

```xml
<Multiply>
  <Rational>60/100</Rational>
  <Dependency path="/agi" />
</Multiply>
```

### `<Divide>`

Evaluates to the `<Dividend>` divided by the product of the `<Divisors>`.

```xml
<Divide>
  <Dividend>
    <Dependency path="../standardAnnualWithholdingAmount" />
  </Dividend>
  <Divisors>
    <Dependency path="../payPeriodsPerYear" />
  </Divisors>
</Divide>
```

### `<Modulo>`

Evaluates to the remainder left after dividing `<Dividend>` by the product of the `<Divisors>`.

```xml
<Modulo>
  <Dependency path="../fullPayPeriods" />
  <Int>2</Int>
</Modulo>
```

### `<Round>`

Evaluates to the nearest integer.

* 4.2 rounds to 4
* 4.5 rounds to 5
* 4.7 rounds to 5

```xml
<Round>
  <Dependency path="../futurePartialPayPeriods" />
</Round>
```

### `<Floor>`

Evaluates to the nearest integer that is greater than the child value.

* 4.2 rounds to 4
* 4.5 rounds to 4
* 4.7 rounds to 4

```xml
<Floor>
  <Dependency path="../futurePartialPayPeriods" />
</Floor>
```

### `<Ceiling>`

Evaluates to the nearest integer that is greater than the child value.

* 4.2 rounds to 5
* 4.5 rounds to 5
* 4.7 rounds to 5

```xml
<Ceiling>
  <Dependency path="../futurePartialPayPeriods" />
</Ceiling>
```

### `<Count>`

Counts the number of its children which evaluate to True.
All children must evaluate to a Boolean.

The example evaluates to 2.

```xml
<Count>
  <True/>
  <True/>
  <False/>
</Count>
```

### `<StepwiseMultiply>`

Stepwise multiply covers a common pattern in the tax code: "add/reduce X by Y for each Z."
This is modeled with X as the `<Multiplicand>`, multiplied by the rate `<Rational>Y/Z</Rational>`.
A full example of the Qualified Tips Deduction Phase-Out is included to show how this is used in practice.

```xml
<Fact path="/qualifiedTipDeductionPhaseOut">
  <Name>Qualified Tip Deduction Phase Out</Name>
  <Description>
    The amount allowable as a deduction under shall be reduced (but not below
    zero) by $100 for each $1,000 by which the taxpayer’s MAGI exceeds $150,000
    ($300,000 in the case of a joint return).


    Example: Single filer with MAGI of $160,500. This amount exceeds the relevant threshold
    ($150,000) by $10,500. This is 10.5 increments of $1,000 in excess of the threshold, which
    is rounded down to 10 increments. Therefore, the otherwise allowable deduction amount is
    reduced by $1,000 ($100/increment multiplied by 10 increments).
  </Description>
  <Derived>
    <StepwiseMultiply>
      <Multiplicand>
        <Dependency path="/qualifiedTipDeductionExcessOverThreshold" />
      </Multiplicand>
      <Rate>
        <!-- Reduce by a rate of $100 for each $1000 of MAGI exceeding the relevant threshold -->
        <Rational>100/1000</Rational>
      </Rate>
    </StepwiseMultiply>
  </Derived>
</Fact>
```

### `<AddPayrollMonths/>`

Adds months to a `<Day>` adjusts the date to be the last day of the month if the original Day was the last day of the month.
This is basically just for withholding calculations.

In the below example, if `mostRecentPayDate` is April 30th, then the expression will evaluate to May 31st.

```xml
<AddPayrollMonths>
  <Dependency path="../mostRecentPayDate" />
  <Int>1</Int>
</AddPayrollMonths>
```

### `<PayrollMonthsBetween />`

The number of months between two days, rounded up (ceiling).
Used to calculate the number of paychecks you have left to receive in a monthly job.

```xml

<PayrollMonthsBetween>
  <StartDate>
    <Dependency path="../startDate" />
  </StartDate>
  <EndDate>
    <Dependency path="../endDate" />
  </EndDate>
</PayrollMonthsBetween>

```

## Collection Expressions

### `<IndexOf>`

Get the ID of an item in a particular collection.
The `<Collection>` element must have a single child that evaluates to a collection.
The `<Index>` element must evaluate to an integer.

```xml
<IndexOf>
  <Collection>
    <Dependency path="/jobs"/>
  </Collection>
  <Index>0</Index>
</IndexOf>
```

> [!NOTE]
> This is a slightly confusing name.
> It's not getting the index of a collection item, it's getting the ID of an item at the provided index.

### `<Maximum>`

Retrieve the maximum value for a certain collection fact across all the items in the collection.
The fact must be collection fact.

Suppose you had three jobs in the jobs collection. Two of them have income values of $500 and $200 respectively, one of them doesn't have an income value.
Then, the below example would evaluate to $500.

```xml
<Maximum>
  <Dependency path="/jobs/*/income" />
</Maximum>
```

### `<Minimum>`

Retrieve the minimum value for a certain collection fact across all the items in the collection.
The fact must be collection fact.

Suppose you had three jobs in the jobs collection. Two of them have income values of $500 and $200 respectively, one of them doesn't have an income value.
Then, the below example would evaluate to $200.

```xml
<Maximum>
  <Dependency path="/jobs/*/income" />
</Maximum>
```

### `<Filter>`

Filter is an important element but also one of the most oddly specified.
It takes a `path` attribute for a collection fact and a child expression that must evaluate to a boolean.
The Filter element evaluates to a collection that only contains elements for which that child expression was true.

The quirky part of the `<Filter>` element is that, within the context of the child expression, you use prefix-less paths to refer to each item X in the collection.
This is is easier to understand by example.

This fact evaluates to the ID of the highest paying job.
It does this by creating a new collection that only contains the highest paying job—or multiple jobs, if the maximum income amount is shared by multiple jobs—and then getting the ID of the first item in that collection.
Note the `<Dependency path="income"/>`, a prefix-less fact path which refers to each fact in `/jobs/*/income`.
The filter's expression will only return true jobs with a `/jobs/*/income` fact that is equal to the maximum income.

```xml
<Fact path="/highestPayingJob">
  <Description>Highest "paying" job available for extra withholding</Description>
  <Derived>
    <IndexOf>
      <Collection>
        <Filter path="/jobs">
          <Equal>
            <Left>
              <Maximum>
                <Dependency path="/jobs/*/income" />
              </Maximum>
            </Left>
            <Right>
              <Dependency path="income" />
            </Right>
          </Equal>
        </Filter>
      </Collection>
      <Index>
        <Int>0</Int>
      </Index>
    </IndexOf>
  </Derived>
</Fact>
```

> [!NOTE]
> I do not like the prefix-less path. It's inconsistent and therefore confusing.
> In my opinion, this could be implemented by using the normal wildcard i.e.
> the `<Right>` element above would have a `<Dependency path="/jobs/*/income">` child.

### `<CollectionSum>`

Sum the value of a particular fact for all collection items.
This example would evaluate to the dollar value of the sum of all incomes.

```xml
<CollectionSum>
  <Dependency path="/jobs/*/income" />
</CollectionSum>
```

### `<CollectionSize>`

The number of items in a collection.
If there are 3 jobs in `/jobs`, this would evaluate to the integer 3.

```xml
<CollectionSize>
  <Dependency path="/jobs" />
</CollectionSize>
```

## Control Flow

### `<Switch>`

The `<Switch>` element allows you to specify multiple cases with a boolean condition and a resulting expression.
Its children comprise multiple `<Case>` elements, each of which has a `<When>` and `<Then>` child.


The `<Switch>` will take on the value of the `<Then>` expression for the first `<Case>` whose `<When>` condition evaluates to true (even with placeholders).
As seen in the example, the `<True>` element is often used as a condition for the last case, to specify a default.

```xml
<Switch>
  <Case>
    <When>
      <Dependency path="../isPastJob" />
    </When>
    <Then>
      <Dollar>0</Dollar>
    </Then>
  </Case>
  <Case>
    <When>
      <True />
    </When>
    <Then>
      <Multiply>
        <Dependency path="../averagePayPerPayPeriodForWithholding" />
        <Dependency path="../remainingPayPeriodsRational" />
      </Multiply>
    </Then>
  </Case>
</Switch>
```

## Deprecated

### Filing-specific types

The Fact Graph includes types like `<Tin>`, `<Ein>`, `<Pin>`,  and `<PhoneNumber>` that I believe should be deprecated.
These are too domain-specific and most of them could be solved with a `<String>` validation mechanism.
Type primitives are expensive to maintain and the Fact Graph should be very cautious about introducing them, especially when they don't have logical implications.

One complex type that *could* have logical implications is the `<Address>` type.
It's easy to imagine that tax logic might want to execute differently based on the taxpayer's US State residence, for instance.
I believe the implementation needs to be re-thought, however, and it's not necessary for the applications the Fact Graph currently supports.

### `<Name>`

This is redundant with either the `<Description>` node and the fact path.
The fact path should be a short name that concisely describes the fact and the `<Description>` should be prose context for that path.
The `<Name>` element exists uneasily between the two, is rarely used; it typically just restates the fact path, with spaces and sentence case.

Existing `<Name>`s should be merged into the description, or simply deleted.
