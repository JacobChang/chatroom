import React from "react";
import "./HomePage.css";
import { Container } from "../Components/Container";
import { ajax } from "rxjs/ajax";
import { Subscription } from "rxjs";
import { SessionContext } from "../Models/Session";
import { IChannel } from "../Models/Channel";
import { Link } from "react-router-dom";

interface Props {}

interface State {
  loading: boolean;
  channels: IChannel[];
}

export class HomePage extends React.Component<Props, State> {
  subscription: Subscription | null = null;

  constructor(props: Props) {
    super(props);

    this.state = {
      loading: true,
      channels: []
    };
  }

  componentDidMount() {
    ajax("/api/v1/channels").subscribe(
      response => {
        const channels = response.response;
        this.setState({
          channels
        });
      },
      error => {
        console.log(error);
      },
      () => {
        this.setState({
          loading: false
        });
      }
    );
  }

  render() {
    const { loading, channels } = this.state;

    return (
      <SessionContext.Consumer>
        {session => {
          return (
            <div className="page page--home">
              <header>
                <Container className={["flex__box", "flex__box--vc"]}>
                  <h3 className="flex__item">Hurry</h3>
                  {session.verified ? (
                    <Link to="/channles/create">Create Channel</Link>
                  ) : (
                    <a
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
                      Join
                    </a>
                  )}
                </Container>
              </header>
              <section>
                <Container>
                  {loading ? (
                    <p>Loading</p>
                  ) : (
                    channels.map(channel => {
                      return (
                        <div className="flex__box flex__box--vc">
                          <div className="flex__item">
                            <p>{channel.title}</p>
                            <span>{channel.duration}</span>
                          </div>
                          <div>
                            <Link to={`/channels/${channel.id}`}>Join</Link>
                          </div>
                        </div>
                      );
                    })
                  )}
                </Container>
              </section>
            </div>
          );
        }}
      </SessionContext.Consumer>
    );
  }
}
