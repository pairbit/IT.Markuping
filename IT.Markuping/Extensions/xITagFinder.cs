using IT.Markuping.Interfaces;
using System;

namespace IT.Markuping.Extensions;

public static class xITagFinder
{
    public static Tags FirstTags<T>(this ITagFinder<T> finder, ReadOnlySpan<T> data, ReadOnlySpan<T> name, out TagNS ns) where T : unmanaged
        => finder.FirstTags(data, name, out ns, out _);

    public static Tags FirstTags<T>(this ITagFinder<T> finder, ReadOnlySpan<T> data, ReadOnlySpan<T> name, ReadOnlySpan<T> ns) where T : unmanaged
        => finder.FirstTags(data, name, ns, out _);

    public static Tags FirstTags<T>(this ITagFinder<T> finder, ReadOnlySpan<T> data, ReadOnlySpan<T> name) where T : unmanaged
        => finder.FirstTags(data, name, out _);

    public static Tags LastTags<T>(this ITagFinder<T> finder, ReadOnlySpan<T> data, ReadOnlySpan<T> name, out TagNS ns) where T : unmanaged
        => finder.LastTags(data, name, out ns, out _);

    public static Tags LastTags<T>(this ITagFinder<T> finder, ReadOnlySpan<T> data, ReadOnlySpan<T> name, ReadOnlySpan<T> ns) where T : unmanaged
        => finder.LastTags(data, name, ns, out _);

    public static Tags LastTags<T>(this ITagFinder<T> finder, ReadOnlySpan<T> data, ReadOnlySpan<T> name) where T : unmanaged
        => finder.LastTags(data, name, out _);

    public static TagOpening FirstTagOpening<T>(this ITagFinder<T> finder, ReadOnlySpan<T> data, ReadOnlySpan<T> name, ReadOnlySpan<T> ns, TagEndings endings = default) where T : unmanaged
        => (TagOpening)finder.FirstTag(data, name, ns, endings.GetEndings());

    public static TagOpening FirstTagOpening<T>(this ITagFinder<T> finder, ReadOnlySpan<T> data, ReadOnlySpan<T> name, TagEndings endings = default) where T : unmanaged
        => (TagOpening)finder.FirstTag(data, name, endings.GetEndings());

    public static TagOpening LastTagOpening<T>(this ITagFinder<T> finder, ReadOnlySpan<T> data, ReadOnlySpan<T> name, ReadOnlySpan<T> ns, TagEndings endings = default) where T : unmanaged
        => (TagOpening)finder.LastTag(data, name, ns, endings.GetEndings());

    public static TagOpening LastTagOpening<T>(this ITagFinder<T> finder, ReadOnlySpan<T> data, ReadOnlySpan<T> name, TagEndings endings = default) where T : unmanaged
        => (TagOpening)finder.LastTag(data, name, endings.GetEndings());

    private static TagEndings GetEndings(this TagEndings endings)
    {
        if (endings == TagEndings.HasNoAttributes)
            return TagEndings.AnyClosingHasNoAttributes;

        if (endings == TagEndings.HasAttributes)
            return TagEndings.AnyClosingHasAttributes;

        return endings;
    }
}