#!/bin/bash
sbt -Dsbt.color=false clean coverage test
sbt -Dsbt.color=false coverageReport
