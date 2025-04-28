#!/bin/bash

sbt clean scalafmt test:scalafmt it/Test/scalafmt scalafmtSbt coverage test it/test coverageReport