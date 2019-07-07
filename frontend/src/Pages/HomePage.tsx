import React from "react";
import "./HomePage.css";
import { Container } from "../Components/Container";
import { ajax } from "rxjs/ajax";
import { Subscription } from "rxjs";
import { SessionContext } from "../Models/Session";
import { IChannel } from "../Models/Channel";
import { Link } from "react-router-dom";
import { PageStatus } from "../Models/PageStatus";
import { Channel } from "../Components/Channel";
import { Toast } from "../Components/Toast";

interface Props {}

interface State {
  status: PageStatus;
  channels: IChannel[];
}

export class HomePage extends React.Component<Props, State> {
  subscription: Subscription | null = null;

  constructor(props: Props) {
    super(props);

    this.state = {
      status: PageStatus.Idle,
      channels: []
    };
  }

  componentDidMount() {
    this.setState({
      status: PageStatus.Loading
    });

    ajax("/api/v1/channels").subscribe(
      response => {
        const channels = response.response;
        this.setState({
          status: PageStatus.Ready,
          channels
        });
      },
      error => {
        this.setState({
          status: PageStatus.Error
        });
      }
    );
  }

  render() {
    const { status, channels } = this.state;

    let content: any = null;
    switch (status) {
      case PageStatus.Idle:
        content = <p>Loading</p>;
        break;
      case PageStatus.Loading:
        content = <p>Loading</p>;
        break;
      case PageStatus.Error:
        content = <p>Error</p>;
        break;
      case PageStatus.Ready:
        content =
          channels.length > 0 ? (
            channels.map(channel => {
              return <Channel key={channel.id} channel={channel} />;
            })
          ) : (
            <div className="placeholder">No Channel</div>
          );
        break;
    }

    return (
      <SessionContext.Consumer>
        {session => {
          return (
            <div className="page page--home">
              <header className="page__header">
                <Container className={["flex__box", "flex__box--vc"]}>
                  <h4 className="flex__item">Hurry</h4>
                  {session.verified ? (
                    <Link
                      className="button button--primary"
                      to="/channels/create"
                    >
                      Create Channel
                    </Link>
                  ) : (
                    <a
                      className="button button--primary"
                      href={`${
                        process.env.REACT_APP_OAUTH_ENDPOINT
                      }?client_id=${
                        process.env.REACT_APP_OAUTH_CLIENT_ID
                      }&server_id=${
                        process.env.REACT_APP_OAUTH_SERVER_ID
                      }&scope_name=user.id&response_type=code&redirect_uri=${encodeURIComponent(
                        process.env.REACT_APP_OAUTH_REDIRECT_URI as string
                      )}&state=stable`}
                    >
                      Sign In
                    </a>
                  )}
                </Container>
              </header>
              <section>
                <Container>{content}</Container>
              </section>
            </div>
          );
        }}
      </SessionContext.Consumer>
    );
  }
}
