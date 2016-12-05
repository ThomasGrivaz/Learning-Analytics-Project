#----------------------------------------#
# Function that computes custom features #
#----------------------------------------#


def CalculateFeatures(VideoEvents=[], ForumEvents=[]):

    # Initialize features dict
    Features = {}

    # Features for video events
    if len(VideoEvents)>0:

        # Calculate custom features
        # Keys: TimeStamp, EventType, VideoID, CurrentTime, OldTime, NewTime, SeekType, OldSpeed, NewSpeed
        EventTypes = VideoEvents['EventType']
        TimeStamps = VideoEvents['TimeStamp']
        TimeStampDiffs = [x[0]-x[1] for x in zip(TimeStamps[1:],TimeStamps[:-1])]
        DurationOfVideoActivity = TimeStamps[-1] - TimeStamps[0]
        AverageVideoTimeDiffs = sum(TimeStampDiffs)/max(1,len(TimeStampDiffs))
        NumberVideoWatched = len(set(VideoEvents['VideoID']))

        NumberOfSeekEvents = EventTypes.count('Video.Seek')

        # Append features to dictionary
        Features.update({
            'DurationOfVideoActivity': DurationOfVideoActivity,
            'AverageVideoTimeDiffs': AverageVideoTimeDiffs,
            'NumberVideoWatched': NumberVideoWatched,
            'NumberOfSeekEvents': NumberOfSeekEvents
        })

    # Features for forum events
    if len(ForumEvents)>0:

        # Calculate custom features
        # Keys: TimeStamp, EventType, PostType, PostID, PostLength
        EventTypes = ForumEvents['EventType']
        NumberOfThreadViews = EventTypes.count('Forum.Thread.View')
        NumberOfPosts = EventTypes.count('Forum.Thread.PostOn')
        NumberOfThreadCreated = EventTypes.count('Forum.Thread.Launch')
        NumberOfComments = EventTypes.count('Forum.Post.CommentOn')
        NumberOfUpvotes = EventTypes.count('Forum.Post.Upvote')+EventTypes.count('Forum.Comment.Upvote')

        # Append features to dictionary
        Features.update({
            'NumberOfThreadViews': NumberOfThreadViews,
            'NumberOfPosts': NumberOfPosts,
            'NumberOfThreadCreated': NumberOfThreadCreated,
            'NumberOfComments': NumberOfComments,
            'NumberOfUpvotes': NumberOfUpvotes
        })
    # video + forum events combined (e.g. n-grams)
    if len(ForumEvents)>0 and len(VideoEvents)>0:
       pass

    return Features