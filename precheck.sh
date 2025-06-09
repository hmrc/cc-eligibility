#!/bin/bash

sbt clean scalafmt Test/scalafmt  scalafmtSbt coverage test coverageReport