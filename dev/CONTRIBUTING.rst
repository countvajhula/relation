Workflow
========

1. Add functionality and tests

2. Update the contracts

3. Update the docs

Build Steps
===========

1. Build package, docs, and check dependencies

.. code-block:: bash

  make build

2. Run tests

.. code-block:: bash

  make test

3. Check dependencies

.. code-block:: bash

  make check-deps

4. View docs

.. code-block:: bash

  make docs

5. Run profiler

.. code-block:: bash

  make profile

Drafting a New Release (Steps for Maintainer)
=============================================

1. When you're ready to cut a new release, bump the version in info.rkt and make a fresh commit

2. Tag the release commit

.. code-block:: bash

  git tag -n  # list existing tags and annotations
  git tag -a <new version number> -m "<release message>"  # or leave out -m to enter it in Vim

3. Push the new tag to origin

.. code-block:: bash

  git push --follow-tags  # push new tag to remote
