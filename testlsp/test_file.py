#!/usr/bin/env python3
"""Test file with security issues for Semgrep to detect."""

import hashlib

# This should trigger a finding about using weak hash (MD5)
def insecure_hash(data):
    return hashlib.md5(data.encode()).hexdigest()

# This should trigger a finding about using weak hash (SHA1)
def weak_hash(data):
    return hashlib.sha1(data.encode()).hexdigest()

# This is safe
def secure_hash(data):
    return hashlib.sha256(data.encode()).hexdigest()

if __name__ == "__main__":
    print(insecure_hash("test"))
    print(weak_hash("test"))
    print(secure_hash("test"))
