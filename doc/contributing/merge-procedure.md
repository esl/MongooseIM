# Merging guidelines

This document aims to provide guidelines for merging changesets into the MongooseIM master branch.

For the purpose of this document, we classify changesets/pull requests as:

* trivial - minor bugfixes, small changes to auxiliary modules, changes to documentation, and patches which change only test code.

* non-trivial - more significant changes, fixes and improvements.
Especially if they change MongooseIM's core modules.

* big - e.g. implementation of new XEPs or major changes to core code

## 1. Preparation

Always create a branch with a descriptive name from an up-to-date master.
Do your work in the branch, push it to the ESL repository if you have access to it, otherwise to your own repository.

When done, run the whole CT suite and write tests related to what you've done.
Then push the changes and create a pull request to master, following the PR description template.
Make sure all Travis tests pass (if only some jobs fail it is advisable to restart them, since they sometimes
fail at random).

If tests fail, see why here: http://mongooseim-ct-results.s3-website-eu-west-1.amazonaws.com/ and resolve any issues.

## 2. Review

Both trivial and non-trivial PRs have to be reviewed by at least one other person from the core development team.
For big changesets consult the Tech Lead.
The reviewer's remarks should be placed as inline comments on github for future reference.

Then, apply the reviewer's suggestions.
If the changes are limited to documentation or code formatting, remember to prefix commit message with "[skip ci]" so as not to run redundant tests.

The reviewer should make sure all of their suggestions are applied.
It is the reviewer who actually does the merge, so he takes at least half of the responsibility.

## 3. Merging

I. If your PR is not a trivial one, always rebase onto master.
This is important, because someone may have merged before which may break your code, and it might be difficult to figure out who should fix it and how.
For the same reason, it is recommended to tell your colleagues that you are about to merge something so that they do not merge at the same time.

II. After rebase, push your branch with -f, make sure all tests pass.

III. Tell your reviewer he can proceed.
He hits the green button, and you can both celebrate.
