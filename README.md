# sensu.el [![Build Status](https://api.travis-ci.org/areina/sensu.el.png?branch=master)](http://travis-ci.org/areina/sensu.el)

Manage [Sensu](http://sensuapp.org/) from Emacs.
The code to manage and work with events is heavily inspired by the great [prodigy.el](http://github.com/rejeep/prodigy.el) project.

## Features

- [x] List current events.
- [x] Resolve one or multiple events.
- [ ] List clients.
- [ ] Un/silence one or multiple clients.

## Installation

Manually right now. I'll add it to melpa soon.

## Configuration

You need to set some values to work with your sensu instance:

- `sensu-api-base-url`
- `sensu-api-auth-user`
- `sensu-api-auth-password`

## Usage

`M-x sensu`
