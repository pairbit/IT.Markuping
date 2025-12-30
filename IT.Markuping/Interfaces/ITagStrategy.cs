using System;

namespace IT.Markuping.Interfaces;

public interface ITagStrategy<T>
{
    int LtLength { get; }

    int LtColonLength { get; }

    int LtSlashLength { get; }

    int LtSlashColonLength { get; }

    int IndexOf(ReadOnlySpan<T> data, ReadOnlySpan<T> name);

    int LastIndexOf(ReadOnlySpan<T> data, ReadOnlySpan<T> name);

    bool IsStartOpening(ReadOnlySpan<T> data, int start);

    bool IsStartOpening(ReadOnlySpan<T> data, int start, ReadOnlySpan<T> ns);

    bool IsStartOpening(ReadOnlySpan<T> data, ref int start, out TagNS ns);

    bool IsStartClosing(ReadOnlySpan<T> data, int start);

    bool IsStartClosing(ReadOnlySpan<T> data, int start, ReadOnlySpan<T> ns);

    bool IsStartClosing(ReadOnlySpan<T> data, ref int start, out TagNS ns);

    bool IsEndClosing(ReadOnlySpan<T> data, ref int end, out bool hasSpace);

    TagEnding GetEndingName(ReadOnlySpan<T> data, ref int end);

    TagEnding GetEndingAttributeStart(ReadOnlySpan<T> data, ref int end);

    TagEnding GetEndingHasAttributes(ReadOnlySpan<T> data, ref int end);
}