from unittest import TestCase

from solution import cost


class TestSolution(TestCase):
    def test_cost_1(self):
        self.assertEqual(cost(1), 2)

    def test_cost_8(self):
        self.assertEqual(cost(8), 9)
