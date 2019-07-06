import React from "react";
import "./Channel.css";
import { IChannel } from "../Models/Channel";
import { Link } from "react-router-dom";

export interface Props {
  channel: IChannel;
}

export class Channel extends React.PureComponent<Props> {
  render() {
    const { channel } = this.props;

    return (
      <div className="channel flex__box">
        <div className="flex__item">
          <p className="channel__title">{channel.title}</p>
          <span className="channel__duration">{channel.duration} minutes</span>
        </div>
        <div>
          <Link
            className="button button--primary button--outline"
            to={`/channels/${channel.id}`}
          >
            Join
          </Link>
        </div>
      </div>
    );
  }
}
