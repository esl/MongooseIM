# Contribution Guidelines

For the purpose of this document, we classify changesets/pull requests as:

* big: implementation of new XEPs or major changes to core code
* non-trivial: significant changes, fixes and improvements. Especially if they change MongooseIM's core modules.
* trivial: minor bugfixes, small changes to auxiliary modules, changes to documentation, and patches which change only test code.

## Writing & Testing Code:

We strive for Inaka's superb [guidelines](https://github.com/inaka/erlang_guidelines).

MongooseIM has an extensive test suite.
Find **unit** (white box) tests under `test/` and **functional** (black box) tests under `big_tests/tests`.
It's important that tests are comprehensible: consider any preconditions, the test itself, and any postconditions that must hold.
Inspect the existing test suites to see how _we_ make these clear.

Write type specifications and function signatures because they're remarkably helpful when it comes to reading the source.

## Documentation & Comments

Place technical documentaion in the directory `doc`.
Find or create a suitable place for your material in terms of which page and section you write it in.
Where documentation is placed is paramount, otherwise regardless of its quality, it will be harder to find it if at all.

What makes a good comment?
Write about why something is done a certain way.
E.g. explain a why a decision was made or describe a subtle but tricky case.
We can read a test case or the source, respectively, to see **what** the code does or **how** it does it.
Comments should give us more insight into **why** something was done (the reasoning).

## 1. Preparation

### Branch and code

Always create a branch with a descriptive name from an up-to-date master.
Do your work in the branch, push it to the ESL repository if you have access to it, otherwise to your own repository.

### Run tests

When done with writing tests related to what you've done, run the whole CT suite, as [described in the docs](https://esl.github.io/MongooseDocs/latest/developers-guide/Testing-MongooseIM/).

### Push

Then push the changes and create a pull request to master, following the PR description template.
Make sure all our CI tests pass (if only some jobs fail it may be advisable to restart them, since they seldom
fail at random).

If the tests fail, you can see why here: https://esl.github.io/circleci-mim-results/s3_reports.html (the exact link is visible under the `Upload results` step) and resolve any issues.

## 2. Review

Both trivial and non-trivial PRs have to be reviewed by at least one person from the development team.

If the changes are limited to documentation or code formatting, remember to prefix commit message with "[skip ci]" so as not to run redundant tests.

The reviewer should make sure all of their suggestions are applied.
It is the reviewer who actually does the merge, so he takes at some of the responsibility.

## 3. Merging

I. If your PR is not a trivial one, always rebase onto master.

This is important, because someone may have merged before which may break your code, and it might be difficult to figure out who should fix it and how.
For the same reason, it is recommended to tell your colleagues that you are about to merge something so that they do not merge at the same time.

II. After rebase, push your branch with -f, make sure all tests pass.

III. Tell your reviewer he can proceed.

IV. He hits the green button, and you can both celebrate.

# Contributing to mobile libraries

We are contributing to:

* XMPPframework for iOS: https://github.com/esl/XMPPFramework/
* Smack for Android: https://github.com/esl/Smack/

The reference respositories are here:

* https://github.com/robbiehanson/XMPPFramework
* https://github.com/igniterealtime/Smack

We highly encourage to go directly to upstream repositories, and follow the relevant guidelines.

# Influencing the MongooseIM roadmap

We do not have a specific process for community roadmap building. You are encouraged and welcome to influence the roadmap with GitHub issues, code contributions, or you can simply [contact us](https://www.erlang-solutions.com/contact/) to invent the future.
