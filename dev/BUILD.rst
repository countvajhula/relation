Build steps
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

Drafting a New Release
======================

1. As part of the release commit, bump the version in info.rkt

2. Tag the release commit

.. code-block:: bash

  git tag -n  # list existing tags and annotations
  git tag -a <new version number> -m "<release message>"  # or leave out -m to enter it in Vim

3. Push the new tag to origin:

.. code-block:: bash

  git push --follow-tags  # push new tag to remote
