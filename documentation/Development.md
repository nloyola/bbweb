# Development

# Snapshot Migration

Snapshots can be considered a pure performance optimization and are not important from a functional point of
view for a successful recovery of application state. That is why it might be a valid alternative to do without
custom serializers and backwards compatibility in case of snapshots. Incompatible snapshots could simply be
deleted in case of an upgrade. Of course this results in a longer recovery time for the first restart after an
upgrade.

## Libraries and Frameworks

### Protocol Buffers

Google [protobuf](https://github.com/google/protobuf/) is used to save events to the Akka Persistence journal.
By using protobuf it is easier to modify the events as the system grows. If an event is changed an old
database can still be used with the application.

See the installation page for instructions on how to install on your computer.

#### ScalaPB

[ScalaPB](http://trueaccord.github.io/ScalaPB/generated-code.html) is used to generate the scala files from
the `proto` files.

## Development Environment

### SBT

More memory is required by SBT when running the server in development mode. See file `.sbtopts` in the
project's root directory.

#### Dependencies

Project [sbt-updated](https://github.com/rtimush/sbt-updates) is used to determine if any
dependencies can be updated. Use command `dependencyUpdates` to display what can be updated.

#### Initialization

Various configuration files need to be created and modified to run the server in development mode or
production mode. Use the following commands to create the configuration files:

```sh
sbt developmentInit
```

#### Configuration

##### Email

The server sends emails for user registration and requires an Email server to be configured. Edit file
`conf/email.conf` and add your emails server information.

##### Test Data

If you want the system to be populated with some test data, edit file `conf/testdata.conf` and choose the test
data to be included on initialization.

##### Testing

See section [Server Logging](#server-logging) for more information.

## Application

### Running

* To start the server in production mode, use the following commands:

    ```sh
    APPLICATION_SECRET="abcdefghijklmnopqrstuvwxyz" sbt start
    ```

    In the browser open the following link: http://localhost:9000/#!/

### Testing

#### Server Tests

Use the command `sbt test` to run the server tests.

##### Scalatest

###### Run one or more tests within a Suite

Use the `-z` flag to run a test with the specified substring:

```sbt
test-only *__CLASS_NAME__ -- -z "sub string"
```

###### Scala code coverage

**sbt-scoverage** is used to determine code coverage. See the
[GitHub page](https://github.com/scoverage/sbt-scoverage)
for instructions on how to use it.

To generate the HTML report use the command:

```sh
sbt clean coverage test
sbt coverageReport
```

Or, within the SBT CLI:

```sh
; clean; coverage; test; coverageReport
```

The report can be found in: `<proj_dir>/target/scala-<version>/scoverage-report`

### Debug

To prevent users being logged out when the application is restarted, EHCACHE is configured to cache
to disk. This must be disabled for the production server (see [conf/ehcache.xml]
(/conf/ehcache.xml), tags: `defaultCache -> diskPersistent`).

### Server Logging

* To enable logging in a production environment, edit the file `conf/logback-play-default.xml`.

* To enable logging in a development environment, edit the file `conf/logback-play-dev.xml`.

* To enable logging in a testing environment, edit the file `conf/logback-test.xml`.

* The Akka logging configuration for the web application is in `conf/application.conf`.

# Souce code documentation

## Server

To generate the documentation from the server source code, run the following command:

```sh
sbt doc
```

The documentation can now be opened in a web browser by opening this link:

```
file:///<_path_to_project_>/target/scala-2.12/api/index.html
```

Where `<_path_to_project_>` is the root directory for the project.
