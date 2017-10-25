"""Script for running `craeftc` integration tests.

A craeftc integration test consists of three parts: a YAML configuration file, a
file containing Craeft code, a C harness, and a file containing expected output.
"""

import os
import subprocess
import tempfile
import traceback
import re

import yaml

DIR = os.path.dirname(__file__)
CRAEFT_COMMAND = ["stack", "exec", "craeft", "--"]
CC = "cc"
CFLAGS = ["-x", "c"]

def temporary_filename():
    (obj, result) = tempfile.mkstemp()
    os.close(obj)
    return result

def try_rm(fname):
    try:
        os.remove(fname)
    except OSError:
        pass

def abs_of_conf_path(fname):
    return os.path.join(DIR, "tests", fname)

def assert_succeeded(args, msg):
    assert subprocess.call(args) == 0, msg

def assert_failed(args, msg):
    assert subprocess.call(args) != 0, msg

class TempFileContextManager(object):
    def registered_tempfile(self):
        result = temporary_filename()
        try:
            self._tempfile_registry.append(result)
        except AttributeError:
            self._tempfile_registry = [result]
        return result

    def __enter__(self):
        return self

    def __exit__(self, exc_type, exc_value, traceback):
        for tempfile in self._tempfile_registry:
            try_rm(tempfile)

class CheckOutputIntegrationTest(TempFileContextManager):
    def __init__(self, parsed):
        """Parse the file named by `fname` into an IntegrationTest."""
        self.name = parsed["name"]

        self.del_code = False
        self.del_harness = False

        def try_file(field):
            try:
                self.__dict__[field] = abs_of_conf_path(parsed[field])
            except KeyError:
                self.__dict__[field] = self.registered_tempfile()
                self.__dict__["del_" + field] = True
                with open(self.__dict__[field], 'w') as f:
                    f.write(parsed[field + "_text"])

        try_file("code")
        try_file("harness")

        try:
            with open(abs_of_conf_path(parsed["output"]), "r") as f:
                self.expected = bytes(f.read(), 'utf-8')
        except KeyError:
            self.expected = bytes(parsed["output_text"], 'utf-8')

        self.code_obj = self.registered_tempfile()
        self.harness_obj = self.registered_tempfile()
        self.exc = self.registered_tempfile()

    def compile_craeft(self):
        assert_succeeded(CRAEFT_COMMAND + [self.code, "--obj", self.code_obj],
                         "craeftc invocation failed")

    def compile_harness(self):
        args = [CC] + CFLAGS
        args += [self.harness, "-c", "-o", self.harness_obj]
        assert_succeeded(args, "compiler invocation failed")

    def link(self):
        assert_succeeded([CC, self.code_obj, self.harness_obj, "-o", self.exc],
                         "compiler linking invocation failed")

    def run_exc(self):
        child = subprocess.Popen([self.exc], stdout=subprocess.PIPE,
                                             stderr=subprocess.PIPE)
        assert child.wait() == 0, ("executable failed with stdout =\n"
                                 + child.stdout.read().decode()
                                 + "\nstderr =\n"
                                 + child.stderr.read().decode())
        found = child.stdout.read()
        msg = "output incorrect: expected {}; found {}".format(self.expected,
                                                               found)
        assert found == self.expected, msg

    def run(self):
        self.compile_craeft()
        self.compile_harness()
        self.link()
        self.run_exc()

class ShouldFailIntegrationTest(TempFileContextManager):
    def __init__(self, parsed):
        self.name = parsed["name"]
        def try_file(field):
            try:
                self.__dict__[field] = abs_of_conf_path(parsed[field])
            except KeyError:
                self.__dict__[field] = self.registered_tempfile()
                self.__dict__["del_" + field] = True
                with open(self.__dict__[field], 'w') as f:
                    f.write(parsed[field + "_text"])

        self.codes = []

        try:
            if isinstance(parsed["code"], list):
                self.codes.extend(parsed["code"])
            else:
                self.codes.append(parsed["code"])
        except KeyError:
            if isinstance(parsed["code_text"], list):
                for code_txt in parsed["code_text"]:
                    newfile = self.registered_tempfile()
                    with open(newfile, "w") as f:
                        f.write(code_txt)
                    self.codes.append(newfile)
            else:
                newfile = self.registered_tempfile()
                with open(newfile, "w") as f:
                    f.write(parsed["code_text"])
                self.codes.append(newfile)

    def run(self):
        for code in self.codes:
            with open(code, 'r') as f:
                msg = "code {} expected to fail".format(f.read())
            args = CRAEFT_COMMAND + [code, "--obj", self.registered_tempfile()]
            assert_failed(args, msg)

def test_factory(fname):
    with open(fname, "r") as f:
        parsed = yaml.load(f)
    try:
        if parsed["type"] == "expect_failure":
            return ShouldFailIntegrationTest(parsed)
        elif parsed["type"] == "compare_output":
            return CheckOutputIntegrationTest(parsed)
        else:
            raise ValueError("type field not recognized")
    except KeyError:
        return CheckOutputIntegrationTest(parsed)


def main():
    contents = os.listdir(os.path.join(DIR, "tests"))
    confs = list(filter(re.compile(".*\.yaml$").match, contents))
    successes = 0
    print("Running tests...")
    for (i, conf) in enumerate(confs):
        fname = os.path.join(DIR, "tests", conf)
        with test_factory(fname) as test:
            prefix = "test {}/{} ({}) ".format(i + 1, len(confs), test.name)
            try:
                test.run()
                successes += 1
                print(prefix + "succeeded.")
            except AssertionError as e:
                print(prefix + "failed. Stack trace:")
                traceback.print_exc()
    print("\nTests complete. {}/{} succeeded.".format(successes, len(confs)))
    if successes != len(confs):
        exit(1)

if __name__ == "__main__":
    main()
