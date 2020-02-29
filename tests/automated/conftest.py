import sys
import os

sys.path.append(os.path.dirname(__file__))


def pytest_addoption(parser):
    parser.addoption(
        '--no-sandbox-root-check', action='store_true', default=False, help='Don\'t do a sandbox root check check'
    )
