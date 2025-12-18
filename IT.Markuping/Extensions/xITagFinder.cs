using IT.Markuping.Interfaces;
using System;

namespace IT.Markuping.Extensions;

public static class xITagFinder
{
    public static TagOpening FirstOpening<T>(this ITagFinder<T> finder, ReadOnlySpan<T> data, ReadOnlySpan<T> name, ReadOnlySpan<T> ns, TagEndings endings = default) where T : unmanaged
        => (TagOpening)finder.First(data, name, ns, endings.GetEndings());

    public static TagOpening FirstOpening<T>(this ITagFinder<T> finder, ReadOnlySpan<T> data, ReadOnlySpan<T> name, TagEndings endings = default) where T : unmanaged
        => (TagOpening)finder.First(data, name, endings.GetEndings());

    public static TagOpening LastOpening<T>(this ITagFinder<T> finder, ReadOnlySpan<T> data, ReadOnlySpan<T> name, ReadOnlySpan<T> ns, TagEndings endings = default) where T : unmanaged
        => (TagOpening)finder.Last(data, name, ns, endings.GetEndings());

    public static TagOpening LastOpening<T>(this ITagFinder<T> finder, ReadOnlySpan<T> data, ReadOnlySpan<T> name, TagEndings endings = default) where T : unmanaged
        => (TagOpening)finder.Last(data, name, endings.GetEndings());

    private static TagEndings GetEndings(this TagEndings endings)
    {
        if (endings == TagEndings.HasNoAttributes)
            return TagEndings.AnyClosingHasNoAttributes;

        if (endings == TagEndings.HasAttributes)
            return TagEndings.AnyClosingHasAttributes;

        return endings;
    }
}