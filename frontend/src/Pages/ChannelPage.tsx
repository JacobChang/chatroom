import React from "react";
import { Container } from "../Components/Container";
import { Link } from "react-router-dom";
import { webSocket } from "rxjs/webSocket";

interface Props {}

interface State {
  title: string;
  duration: number;
  max_members: number;
}

export class ChannelPage extends React.Component<Props, State> {
  componentDidMount() {
    webSocket("http://hurry.feblr.org/websocket").subscribe(data => {
      console.log(data);
    });
  }

  render() {
    return (
      <div className="page page--channel">
        <header>
          <Container className={["flex__box", "flex__box--vc"]}>
            <h4 className="flex__item">
              <Link to="/">Back to Home</Link>
            </h4>
          </Container>
        </header>
        <section>
          <Container>
            <p>Channel Page</p>
          </Container>
        </section>
      </div>
    );
  }
}
