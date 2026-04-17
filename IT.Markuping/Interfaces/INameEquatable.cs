using System;

namespace IT.Markuping.Interfaces;

public interface INameEquatable
{
    public bool Equals<T>(ReadOnlySpan<T> name) where T : unmanaged;
}