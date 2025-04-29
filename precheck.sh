#!/bin/bash

sbt clean scalafmt test:scalafmt  scalafmtSbt coverage test coverageReport