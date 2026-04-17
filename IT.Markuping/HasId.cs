using System;

namespace IT.Markuping;

public delegate bool HasId<T>(ReadOnlySpan<T> name);