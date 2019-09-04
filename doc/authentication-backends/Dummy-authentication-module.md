## Overview

The purpose of this module is to make it possible to authenticate a user without
the need for real authentication. In other words, using this module allows to
connect any user to the server without providing any password,
certificate, etc.

From a more detailed perspective, the backend just accepts every
authentication attempt and introduces a random delay (50-500ms) to
an authorization response.

This kind of authorization sometimes really comes in handy, especially during
development and testing.
