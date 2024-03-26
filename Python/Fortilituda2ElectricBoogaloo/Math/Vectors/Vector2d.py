import math
from Math.Vectors.VectorNd import VectorNd


class Vector2d(VectorNd):
    def __init__(self, x, y):
        super().__init__(x, y)

    def rotate_counter_clockwise(self, degrees):
        degrees *= math.pi / 180

        return Vector2d(self.values[0] * math.cos(degrees) - self.values[1] * math.sin(degrees),
                        self.values[0] * math.sin(degrees) + self.values[1] * math.cos(degrees))

    def rotate_clockwise(self, degrees):
        degrees *= math.pi / 180

        return Vector2d(self.values[0] * math.cos(degrees) + self.values[1] * math.sin(degrees),
                        self.values[0] * -math.sin(degrees) + self.values[1] * math.cos(degrees))
