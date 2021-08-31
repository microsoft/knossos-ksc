# mypy: ignore-errors
import abc
from typing import Iterator, Mapping, Sequence, Tuple, Optional, List
from typing import TypeVar
from sortedcontainers import SortedDict

E = TypeVar("E")  # element


class CumSequence(Sequence[E]):
    """
    ABC for Cumulative max/min sequences that efficiently store cumulative min/maxs.

    See CumMaxSequence, CumMinSequence for implementations. The below documentation is
    for CumMaxSequence, though similar ideas hold for CumMinSequence.

    This structure ensures seq[j] >= seq[i] if j > i. It does this efficiently by only
    maintaining indices/values at critical change points. Values for other indices are
    given by the value associated with the next smallest index present. For example, if
    you have "critical values" {0: 2.3, 5: 4.5}, this corresponds to
    [2.3, 2.3, 2.3, 2.3, 4.5].

    Accumulation occurs over both index and time, i.e. assigning a lower value than is
    already present at the index will not result in any change.

    ```python
    seq = CumMaxSequence()
    seq.update(0, 5)
    seq.update(0, 4)
    assert seq[0] == 5
    ```

    Changing values will affect lower values at higher indices.

    ```python
    seq = CumMaxSequence()
    seq.update(0, 2)
    seq.update(2, 4)
    seq.update(3, 10)
    seq.update(1, 7)  # removes seq[2] from critical values
    assert list(seq) == [2, 7, 7, 10]

    ```

    The length is based on the largest critical index. This can sometimes change
    unexpectedly, e.g.

    ```python
    seq = CumMaxSequence()
    seq.update(0, 3)
    seq.update(5, 7)
    assert seq.to_list() == [3, 3, 3, 3, 3, 7]
    assert len(seq) == 6
    seq.update(2, 7)  # removes value at index 5 because it's the same
    assert seq.critical_values == {0: 3, 2: 7}
    assert len(seq) == 3
    assert seq.to_list() == [3, 3, 7]
    assert seq.to_list(6) == [3, 3, 7, 7, 7, 7]
    ```

    Critical indices/values can be accessed through `CumMaxSequence.critical_values`.
    This should be treated as an immutable Mapping.
    """

    class _HoleyIter(Iterator[E]):
        """
        `CumSequence` iterator.

        Note that while `CumSequence` only stores sparse critical values, this iterator
        returns elements of the dense representation.

        See `CumSequence.__iter__` for use. `items_iter` must have at least one entry.
        """

        def __init__(self, items_iter: Iterator[Tuple[int, E]]):
            self._iter = items_iter
            self._index, self._curr_value = next(self._iter)
            if self._index != 0:
                raise ValueError(f"No value for index 0 - first index is {self._index}")
            self._finished = False
            self._advance()

        def _advance(self):
            try:
                self._next_index, self._next_value = next(self._iter)
            except StopIteration:
                self._next_index = self._index + 1
                self._finished = True

        def __next__(self):
            if self._index == self._next_index:
                if self._finished:
                    raise StopIteration()
                self._curr_value = self._next_value
                self._advance()
            self._index += 1
            return self._curr_value

    def __init__(self):
        self._critical_values: Mapping[int, E] = SortedDict()

    @classmethod
    def from_critical_values(cls, critical_values: Mapping[int, E]) -> "CumSequence[E]":
        out = cls()
        out._critical_values.update(critical_values)
        return out

    def copy(self):
        return self.__class__.from_critical_values(self._critical_values)

    @abc.abstractmethod
    def _skip_update(self, curr_val: E, new_val: E) -> bool:
        raise NotImplementedError("Abstract method")

    def _propagate(self, key_index, value):
        items = self._critical_values.items()[key_index:]
        for k, v in items:
            if self._skip_update(value, v):
                del self._critical_values[k]
            else:
                break

    def __getitem__(self, index: int) -> E:
        if index in self._critical_values:
            return self._critical_values[index]
        if index < 0:
            raise IndexError(f"index must be non-negative, got {index}")
        bisect_index = self._critical_values.bisect(index)
        if bisect_index == 0:
            raise IndexError(f"index {index} smaller than current minimum")
        return self._critical_values.values()[bisect_index - 1]

    def __iter__(self):
        if len(self._critical_values) == 0:
            return iter(())
        return CumSequence._HoleyIter(iter(self._critical_values.items()))

    def update_from_seq(self, other: "CumSequence") -> bool:
        # This'll only be sensible if "other" is of the same type (same subclass of CumSequence) as self
        is_updated = [self.update(k, v) for k, v in other.critical_values.items()]
        return any(is_updated)

    def update(self, index: int, value: E) -> bool:
        """
        Attempt to update value at index.

        This update will be ignored if the current value at index (or the closest
        earlier index if not present) is larger / smaller than value for
        CumMaxSequence / CumMinSequence respectively.

        Args:
            index: integer position in the dense representation to potentially set
                value.
            value: value to set

        Returns:
            bool indicating whether or not the update was applied.
        """
        if index < 0:
            raise IndexError(f"index must be non-negative, got {index}")
        if index in self._critical_values:
            curr_val = self._critical_values[index]
            if self._skip_update(curr_val, value):
                return False
            right_index = self._critical_values.bisect(index)
        else:
            right_index = self._critical_values.bisect(index)
            if right_index > 0:
                left_val = self._critical_values[
                    self._critical_values.keys()[right_index - 1]
                ]
                if self._skip_update(left_val, value):
                    # skip if the left value is bigger
                    return False
            right_index += 1
        self._critical_values[index] = value
        self._propagate(right_index, value)
        return True

    def __len__(self) -> int:
        keys = self._critical_values.keys()
        return 0 if len(keys) == 0 else (keys[-1] + 1)

    def to_list(self, length: Optional[int] = None) -> List[E]:
        """Get a cropped or padded list representation with missing values filled in."""
        out = list(self)
        if length is None:
            return out
        padding = length - len(out)
        return out[:length] + out[-1:] * padding

    @property
    def critical_values(self) -> Mapping[int, E]:
        return self._critical_values


class CumMaxSequence(CumSequence[E]):
    def _skip_update(self, curr_val: E, new_val: E) -> bool:
        return new_val <= curr_val


class CumMinSequence(CumSequence[E]):
    def _skip_update(self, curr_val: E, new_val: E) -> bool:
        return new_val >= curr_val
