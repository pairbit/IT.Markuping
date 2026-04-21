using System;

namespace IT.Markuping.Interfaces;

public interface IAttName
{
    public bool Equals<T>(ReadOnlySpan<T> tagName, ReadOnlySpan<T> attName, ReadOnlySpan<T> dtd) where T : unmanaged;
}