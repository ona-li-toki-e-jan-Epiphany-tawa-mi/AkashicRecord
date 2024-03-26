import math


class VectorNd:
    def __init__(self, *args):
        if len(args) > 0 and (type(args[0]) == list or type(args[0]) == tuple):
            args = args[0]

        self.values = []

        for value in args:
            if type(value) == float or type(value) == int:
                self.values.append(value)

            else:
                raise Exception(str(value) + " is not a number. Values of vectors must be numbers!")

    def get_magnitude(self):
        length = 0

        # sqrt(self[0]^2 + self[1]^2 + ... + self[n]^2)
        for value in self.values:
            length += math.pow(value, 2)

        return math.sqrt(length)

    def normalize(self):
        magnitude = self.get_magnitude()

        new_values = []

        if magnitude > 0:
            for i in range(0, self.get_dimension()):
                new_values.append(self.values[i] / magnitude)

        return VectorNd(new_values)

    def get_dimension(self):
        return len(self.values)

    # Adds vectors with vectors.
    def __add__(self, other):
        if type(other) == tuple or type(other) == list:
            if len(other) != self.get_dimension():
                raise Exception(str(self.get_dimension()) + ", the dimension of the vector " + str(self) + ", is not equal to " + str(len(other)) + ", the length of " + str(other) +
                                ". The dimension of a vector and the length of a list must be equal for addition.")

            new_values = []

            for i in range(0, self.get_dimension()):
                new_values.append(self.values[i] + other[i])

            return VectorNd(new_values)

        elif type(other) == VectorNd:
            if other.get_dimension() != self.get_dimension():
                raise Exception(str(self.get_dimension()) + ", the dimension of the vector " + str(self) + ", is not equal to " + str(other.get_dimension()) + ", the length of " + str(other) +
                                ". The dimensions of two vectors must be equal for addition.")

            new_values = []

            for i in range(0, self.get_dimension()):
                new_values.append(self.values[i] + other.values[i])

            return VectorNd(new_values)

        else:
            raise Exception(str(other) + " is not a tuple, list, or vector. Vectors can only be added with tuples, lists, and other vectors.")

    # Subtracts vectors by vectors.
    def __sub__(self, other):
        if type(other) == tuple or type(other) == list:
            if len(other) != self.get_dimension():
                raise Exception(str(self.get_dimension()) + ", the dimension of the vector " + str(self) + ", is not equal to " + str(len(other)) + ", the length of " + str(other) +
                                ". The dimension of a vector and the length of a list must be equal for subtraction.")

            new_values = []

            for i in range(0, self.get_dimension()):
                new_values.append(self.values[i] - other[i])

            return VectorNd(new_values)

        elif type(other) == VectorNd:
            if other.get_dimension() != self.get_dimension():
                raise Exception(str(self.get_dimension()) + ", the dimension of the vector " + str(self) + ", is not equal to " + str(other.get_dimension()) + ", the dimension of " + str(other) +
                                ". The dimensions of two vectors must be equal for subtraction.")

            new_values = []

            for i in range(0, self.get_dimension()):
                new_values.append(self.values[i] - other.values[i])

            return VectorNd(new_values)

        else:
            raise Exception(str(
                other) + " is not a tuple, list, or vector. Vectors can only be subtracted by tuples, lists, and other vectors.")

    # Scales vectors and gets dot products of vectors.
    def __mul__(self, other):
        # Scaling.
        if type(other) == float or type(other) == int:
            new_values = []

            for i in range(0, self.get_dimension()):
                new_values.append(self.values[i] * other)

            return VectorNd(new_values)

        # Dot product.
        elif type(other) == VectorNd:
            if other.get_dimension() != self.get_dimension():
                raise Exception(str(self.get_dimension()) + ", the dimension of the vector " + str(self) + ", is not equal to " + str(other.get_dimension()) + ", the dimension of " + str(other) +
                                ". The dimensions of two vectors must be equal for dot product.")

            dot_product = 0

            for i in range(0, self.get_dimension()):
                dot_product += self.values[i] * other.values[i]

            return dot_product

        # Dot product.
        elif type(other) == list or type(other) == tuple:
            if len(other) != self.get_dimension():
                raise Exception(str(self.get_dimension()) + ", the dimension of the vector " + str(self) + ", is not equal to " + str(len(other)) + ", the length of " + str(other) +
                                ". The dimension of a vector and the length of a list must be equal for dot product.")

            dot_product = 0

            for i in range(0, self.get_dimension()):
                dot_product += self.values[i] * other[i]

            return dot_product

        else:
            raise Exception(str(other) + " is not a number, list, tuple, or a vector. The scalar must be a number for scaling! The second operand must be a list, tuple, or both operands must be"
                                         " vectors for dot product!")

    # Scales vectors.
    def __truediv__(self, other):
        if not type(other) == float and not type(other) == int:
            raise Exception(str(other) + " is not a number. A scalar must be a number!")

        if other == 0:
            raise ZeroDivisionError("division by zero is undefined!")

        new_values = []

        for i in range(0, self.get_dimension()):
            new_values.append(self.values[i] / other)

        return VectorNd(new_values)

    # Scales vectors.
    def __floordiv__(self, other):
        if not type(other) == float and not type(other) == int:
            raise Exception(str(other) + " is not a number. A scalar must be a number!")

        if other == 0:
            raise ZeroDivisionError("division by zero is undefined!")

        new_values = []

        for i in range(0, self.get_dimension()):
            new_values.append(self.values[i] // other)

        return VectorNd(new_values)

    def __str__(self):
        return str(self.values)
